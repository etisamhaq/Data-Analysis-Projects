"""
Two kinds of model, kept deliberately separate:

1. **Explanatory** (`fit_factor_weights`): which Four Factors separate winners from losers, using statistics
   from the game itself. Great for understanding, useless for forecasting.
2. **Predictive** (`evaluate_predictors`): win probability before tip-off, from Elo and recent form only,
   judged on later seasons it never saw, against simple baselines.

Probabilities are scored with log loss and Brier score as well as accuracy, because a forecast that says
51% and one that says 90% are both "right" when the favourite wins, but only one of them is useful.
"""

from __future__ import annotations

from dataclasses import dataclass

import numpy as np
import pandas as pd
from sklearn.ensemble import HistGradientBoostingClassifier
from sklearn.linear_model import LogisticRegression
from sklearn.metrics import accuracy_score, brier_score_loss, log_loss, roc_auc_score
from sklearn.pipeline import make_pipeline
from sklearn.preprocessing import StandardScaler

from .four_factors import FACTORS, LABELS

SEED = 42


# ---------------------------------------------------------------- explanatory model

def fit_factor_weights(home: pd.DataFrame) -> pd.DataFrame:
    """Standardised logistic-regression coefficients of win on the four factor gaps, as relative weights."""
    X = home[["diff_" + f for f in FACTORS]]
    model = make_pipeline(StandardScaler(), LogisticRegression()).fit(X, home["win"])
    coef = model[-1].coef_[0]
    weights = np.abs(coef) / np.abs(coef).sum() * 100
    return pd.DataFrame({"coefficient_per_sd": coef, "relative_weight_pct": weights},
                        index=[LABELS[f] for f in FACTORS])


# ---------------------------------------------------------------- predictive models

@dataclass
class Split:
    train: pd.DataFrame
    validation: pd.DataFrame
    test: pd.DataFrame


def time_split(data: pd.DataFrame, train_last: int, validation_last: int) -> Split:
    """Train on seasons <= train_last, tune on the next ones up to validation_last, test on the rest."""
    return Split(train=data[data["season"] <= train_last],
                 validation=data[data["season"].between(train_last + 1, validation_last)],
                 test=data[data["season"] > validation_last])


def score(y: pd.Series, p: np.ndarray) -> dict[str, float]:
    p = np.clip(p, 1e-6, 1 - 1e-6)
    return {"accuracy": accuracy_score(y, p > 0.5), "log_loss": log_loss(y, p),
            "brier": brier_score_loss(y, p), "auc": roc_auc_score(y, p) if y.nunique() > 1 else np.nan}


def candidate_models(features: list[str]) -> dict[str, object]:
    return {
        "Logistic regression (Elo + form)": make_pipeline(StandardScaler(), LogisticRegression(C=0.1, max_iter=1000)),
        "Gradient boosting (Elo + form)": HistGradientBoostingClassifier(
            max_iter=300, learning_rate=0.03, max_depth=3, min_samples_leaf=200, l2_regularization=1.0,
            early_stopping=False, random_state=SEED),
    }


def evaluate_predictors(data: pd.DataFrame, features: list[str], split: Split) -> tuple[pd.DataFrame, dict]:
    """
    Score baselines and models on validation and test seasons.

    Models are fitted on `split.train` only. Rows missing a feature (each team's first games of a season,
    before it has three games of form) are dropped from every model *and* baseline so all are scored on the
    same games.
    """
    usable = data.dropna(subset=features)
    parts = {name: usable[usable["gameId"].isin(getattr(split, name)["gameId"])]
             for name in ("train", "validation", "test")}
    home_rate = parts["train"]["win"].mean()

    predictions: dict[str, dict[str, np.ndarray]] = {
        "Home team always wins (base rate)": {k: np.full(len(v), home_rate) for k, v in parts.items()},
        "Elo rating only": {k: v["p_home_elo"].to_numpy() for k, v in parts.items()},
    }
    fitted = {}
    for name, model in candidate_models(features).items():
        model.fit(parts["train"][features], parts["train"]["win"])
        fitted[name] = model
        predictions[name] = {k: model.predict_proba(v[features])[:, 1] for k, v in parts.items()}

    rows = []
    for name, preds in predictions.items():
        for part in ("validation", "test"):
            rows.append({"model": name, "split": part, "games": len(parts[part]),
                         **score(parts[part]["win"], preds[part])})
    table = pd.DataFrame(rows).set_index(["split", "model"]).sort_index()
    return table, {"models": fitted, "predictions": predictions, "parts": parts}


def calibration_table(y: pd.Series, p: np.ndarray, bins: int = 10) -> pd.DataFrame:
    """Predicted vs observed win rate by probability bin: a calibrated model sits on the diagonal."""
    frame = pd.DataFrame({"y": np.asarray(y), "p": p})
    frame["bin"] = pd.cut(frame["p"], np.linspace(0, 1, bins + 1), include_lowest=True)
    table = frame.groupby("bin", observed=True).agg(predicted=("p", "mean"), observed=("y", "mean"), games=("y", "size"))
    return table[table["games"] >= 30]


def bootstrap_metric_gap(y: pd.Series, p_a: np.ndarray, p_b: np.ndarray, metric: str = "log_loss",
                         n_boot: int = 1000, seed: int = SEED) -> tuple[float, float, float]:
    """Mean and 95% interval of metric(a) - metric(b) over bootstrap resamples of games."""
    rng = np.random.default_rng(seed)
    y = np.asarray(y)
    fn = {"log_loss": lambda yy, pp: log_loss(yy, np.clip(pp, 1e-6, 1 - 1e-6), labels=[0, 1]),
          "brier": brier_score_loss}[metric]
    gaps = []
    for _ in range(n_boot):
        i = rng.integers(0, len(y), len(y))
        gaps.append(fn(y[i], p_a[i]) - fn(y[i], p_b[i]))
    gaps = np.array(gaps)
    return float(gaps.mean()), float(np.percentile(gaps, 2.5)), float(np.percentile(gaps, 97.5))
