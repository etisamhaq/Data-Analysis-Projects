"""
Business analyses on top of the warehouse marts.

Each function answers one question and returns plain DataFrames/dicts, so the notebook, the dashboard and the
tests all use the same numbers.
"""

from __future__ import annotations

import numpy as np
import pandas as pd
import statsmodels.formula.api as smf
from sklearn.linear_model import LogisticRegression
from sklearn.metrics import average_precision_score, roc_auc_score
from sklearn.pipeline import make_pipeline
from sklearn.preprocessing import StandardScaler

from .warehouse import query

WINDOW = ("2017-01-01", "2018-08-01")   # complete months
SEED = 42


def delivered_reviewed_orders(con) -> pd.DataFrame:
    df = query(con, f"""
        select * from fct_orders
        where is_delivered and review_score is not null
          and purchase_month between date '{WINDOW[0]}' and date '{WINDOW[1]}'""")
    df["late_i"] = df["late"].astype(int)
    df["log_value"] = np.log1p(df["order_value"])
    df["freight_share"] = df["freight_value"] / df["order_value"]
    df["multi_seller"] = (df["sellers"] > 1).astype(int)
    df["month"] = df["purchase_month"].astype(str)
    return df


def yoy_growth(kpis: pd.DataFrame) -> dict:
    """Jan-Aug 2018 vs Jan-Aug 2017: the longest like-for-like comparison the data allows."""
    k = kpis.assign(month=pd.to_datetime(kpis["month"]))
    a = k[(k["month"] >= "2017-01-01") & (k["month"] <= "2017-08-01")]
    b = k[(k["month"] >= "2018-01-01") & (k["month"] <= "2018-08-01")]
    return {"gmv_2017": float(a["gmv"].sum()), "gmv_2018": float(b["gmv"].sum()),
            "gmv_growth": float(b["gmv"].sum() / a["gmv"].sum() - 1),
            "orders_growth": float(b["orders"].sum() / a["orders"].sum() - 1),
            "aov_2017": float(a["gmv"].sum() / a["orders"].sum()), "aov_2018": float(b["gmv"].sum() / b["orders"].sum())}


def late_review_effect(orders: pd.DataFrame) -> pd.DataFrame:
    """
    Effect of a late delivery on the review score: raw difference, then OLS controlling for what else could
    make late orders score lower (category, customer state, month, value, freight share, item and seller count).
    Heteroskedasticity-robust (HC1) standard errors.
    """
    raw = smf.ols("review_score ~ late_i", orders).fit(cov_type="HC1")
    controlled = smf.ols("review_score ~ late_i + log_value + freight_share + items + multi_seller"
                         " + C(main_category) + C(customer_state) + C(month)", orders).fit(cov_type="HC1")
    rows = []
    for name, fit in [("Raw difference", raw), ("With controls", controlled)]:
        low, high = fit.conf_int().loc["late_i"]
        rows.append({"model": name, "effect_stars": fit.params["late_i"], "ci_low": low, "ci_high": high,
                     "orders": int(fit.nobs)})
    return pd.DataFrame(rows).set_index("model")


def lateness_dose_response(orders: pd.DataFrame) -> pd.DataFrame:
    """Review outcomes by how many days late the order arrived."""
    buckets = pd.cut(orders["days_late"], [-1, 0, 3, 7, 14, 10_000], labels=["On time", "1-3 days", "4-7 days",
                                                                             "8-14 days", "15+ days"])
    return orders.groupby(buckets, observed=True).agg(
        orders=("order_id", "size"), avg_review=("review_score", "mean"),
        low_review_share=("review_score", lambda s: (s <= 2).mean()))


def first_orders_with_followup(con, days: int = 180) -> pd.DataFrame:
    """
    Each customer's first valid, delivered, reviewed order, with whether they ordered again within `days`.
    Only first orders placed early enough to have a full `days` of follow-up are kept, so every customer has
    the same chance to come back.
    """
    last_purchase = query(con, "select max(purchased_at) as t from fct_orders where is_valid")["t"][0]
    cutoff = pd.Timestamp(last_purchase) - pd.Timedelta(days=days)
    df = query(con, f"""
        with firsts as (select * from fct_orders where is_valid and order_number = 1),
        second as (select customer_unique_id, min(purchased_at) as second_at
                   from fct_orders where is_valid and order_number = 2 group by 1)
        select f.*, coalesce(s.second_at <= f.purchased_at + interval {days} day, false) as repeat
        from firsts f left join second s using (customer_unique_id)
        where f.purchased_at >= timestamp '{WINDOW[0]}' and f.purchased_at <= timestamp '{cutoff}'
          and f.is_delivered and f.review_score is not null""")
    df["repeat"] = df["repeat"].astype(int)
    df["late_i"] = df["late"].astype(int)
    df["log_value"] = np.log1p(df["order_value"])
    states = df["customer_state"].value_counts()
    df["state_group"] = np.where(df["customer_state"].isin(states[states >= 500].index), df["customer_state"], "other")
    return df


def repeat_after_late(first_orders: pd.DataFrame) -> dict:
    """Does a late first delivery lower the chance of a second order? Logistic regression with controls."""
    fit = smf.logit("repeat ~ late_i + log_value + C(state_group) + C(purchase_month)", first_orders).fit(
        disp=0, method="bfgs", maxiter=2000)
    if not fit.mle_retvals["converged"]:
        raise RuntimeError("Repeat-purchase model did not converge")
    low, high = np.exp(fit.conf_int().loc["late_i"])
    rates = first_orders.groupby("late_i")["repeat"].mean()
    return {"repeat_rate_on_time": float(rates[0]), "repeat_rate_late": float(rates[1]),
            "odds_ratio": float(np.exp(fit.params["late_i"])), "ci_low": float(low), "ci_high": float(high),
            "p_value": float(fit.pvalues["late_i"]), "customers": int(fit.nobs)}


REPEAT_FEATURES = ["log_value", "late_i", "review_score", "delivery_days", "items", "installments", "freight_value"]


def repeat_features(df: pd.DataFrame, states: list[str], categories: list[str]) -> pd.DataFrame:
    x = df[REPEAT_FEATURES].astype(float).fillna(0)
    for s in states:
        x[f"state_{s}"] = (df["customer_state"] == s).astype(int)
    for c in categories:
        x[f"category_{c}"] = (df["main_category"] == c).astype(int)
    return x


def repeat_propensity_model(first_orders: pd.DataFrame, split_date: str = "2017-09-01") -> dict:
    """
    Can first-order information identify likely repeat buyers? Train on customers acquired before `split_date`,
    test on those acquired after (a time split, as the model would be used). Reports AUC, average precision and
    the repeat rate in the top-scored decile versus the base rate.
    """
    train = first_orders[first_orders["purchased_at"] < split_date]
    test = first_orders[first_orders["purchased_at"] >= split_date]
    states = train["customer_state"].value_counts().loc[lambda s: s >= 500].index.tolist()
    categories = train["main_category"].value_counts().head(15).index.tolist()
    model = make_pipeline(StandardScaler(), LogisticRegression(max_iter=2000, C=0.5, random_state=SEED))
    model.fit(repeat_features(train, states, categories), train["repeat"])
    p = model.predict_proba(repeat_features(test, states, categories))[:, 1]
    top = p >= np.quantile(p, 0.9)
    return {"train_customers": len(train), "test_customers": len(test), "auc": roc_auc_score(test["repeat"], p),
            "average_precision": average_precision_score(test["repeat"], p), "base_rate": float(test["repeat"].mean()),
            "top_decile_repeat_rate": float(test.loc[top, "repeat"].mean()),
            "top_decile_lift": float(test.loc[top, "repeat"].mean() / test["repeat"].mean())}
