"""
Year-by-year shares with confidence intervals and significance tests.

A share that moves from 25% to 34% sounds like a trend; with a few hundred titles a year it could be noise.
Every headline change is therefore reported with a 95% interval and a test.
"""

from __future__ import annotations

import numpy as np
import pandas as pd
import statsmodels.api as sm
from scipy import stats
from statsmodels.stats.proportion import proportion_confint


def share_by_year(df: pd.DataFrame, flag: str, year: str = "year_added") -> pd.DataFrame:
    """Share of rows where `flag` is 1, per year, with Wilson 95% intervals."""
    # Plain float/int output: nullable (Int64/Float64) columns break plotting and some statistics libraries
    g = df.groupby(year)[flag].agg(["sum", "count"]).astype(float)
    g.index = g.index.astype(int)
    low, high = proportion_confint(g["sum"], g["count"], method="wilson")
    return pd.DataFrame({"share_%": g["sum"] / g["count"] * 100, "ci_low_%": low * 100, "ci_high_%": high * 100,
                         "n": g["count"].astype(int)})


def trend_test(df: pd.DataFrame, flag: str, first: int, last: int, year: str = "year_added") -> dict:
    """Logistic regression of `flag` on year: odds ratio per year and its p-value over first..last."""
    d = df[df[year].between(first, last)]
    x = sm.add_constant((d[year] - first).astype(float))
    fit = sm.Logit(d[flag].astype(float), x).fit(disp=0)
    low, high = np.exp(fit.conf_int().iloc[1])
    return {"odds_ratio_per_year": float(np.exp(fit.params.iloc[1])), "ci_low": float(low), "ci_high": float(high),
            "p_value": float(fit.pvalues.iloc[1]), "titles": int(len(d)), "years": f"{first}-{last}"}


def compare_years(df: pd.DataFrame, flag: str, year_a: int, year_b: int, year: str = "year_added") -> dict:
    """Chi-square test that a share differs between two years."""
    d = df[df[year].isin([year_a, year_b])]
    table = pd.crosstab(d[year], d[flag])
    p = stats.chi2_contingency(table)[1]
    shares = d.groupby(year)[flag].mean() * 100
    return {"from": year_a, "to": year_b, "share_from_%": float(shares[year_a]), "share_to_%": float(shares[year_b]),
            "p_value": float(p)}


def share_change(items: pd.DataFrame, column: str, before: tuple[int, int], after: tuple[int, int],
                 year: str = "year_added") -> pd.Series:
    """Change in each item's share of mentions (percentage points) between two year windows."""
    share = pd.crosstab(items[year], items[column], normalize="index") * 100
    return (share.loc[after[0]:after[1]].mean() - share.loc[before[0]:before[1]].mean()).sort_values()
