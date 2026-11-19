"""
Individual checks for signs that a tabular dataset was generated at random rather than recorded.

Each check looks for one kind of evidence and returns a `CheckResult`. A single triggered check is weak
evidence: real data can be balanced by design (home/away), uniform by construction (IDs) or written in another
language. The verdict in `report.py` therefore needs several *independent kinds* of evidence.

Thresholds were calibrated on two known-synthetic datasets (Kaggle cybersecurity attacks, air quality) and
three real ones (Netflix titles, NBA box scores, NSE stock prices); `docs/methodology.md` lists the values seen.
"""

from __future__ import annotations

import re
from dataclasses import dataclass, field

import numpy as np
import pandas as pd
from scipy import stats

MIN_ROWS = 500


@dataclass
class CheckResult:
    check: str
    triggered: bool
    summary: str
    evidence: list[dict] = field(default_factory=list)
    applicable: bool = True


def _not_applicable(check: str, reason: str) -> CheckResult:
    return CheckResult(check, False, reason, applicable=False)


def _continuous_columns(df: pd.DataFrame, min_distinct: int = 50) -> list[str]:
    """Numeric columns with enough distinct values to behave like measurements; skips row-ID-like columns."""
    cols = []
    for c in df.select_dtypes("number").columns:
        s = df[c].dropna()
        if s.nunique() <= min_distinct:
            continue
        if s.is_unique and s.is_monotonic_increasing:   # row IDs are uniform by construction
            continue
        cols.append(c)
    return cols


def _text_columns(df: pd.DataFrame) -> list[str]:
    return list(df.select_dtypes(include=["object", "string"]).columns)


# ---------------------------------------------------------------- 1. evenly split categories

def even_categories(df: pd.DataFrame, max_ratio: float = 1.15, min_columns: int = 2) -> CheckResult:
    """
    Categorical columns (3-20 levels) whose most common level is at most `max_ratio` times as common as the
    least common. Real categories are almost always lopsided; random draws are nearly perfectly even.
    """
    name = "Evenly split categories"
    evidence = []
    for c in _text_columns(df):
        s = df[c].dropna()
        if len(s) < MIN_ROWS or not 3 <= s.nunique() <= 20:
            continue
        shares = s.value_counts(normalize=True)
        ratio = shares.max() / shares.min()
        if ratio <= max_ratio:
            evidence.append({"column": c, "levels": int(s.nunique()), "max_min_ratio": round(float(ratio), 3)})
    triggered = len(evidence) >= min_columns
    return CheckResult(name, triggered, f"{len(evidence)} categorical column(s) split almost perfectly evenly", evidence)


# ---------------------------------------------------------------- 2. flat numeric distributions

def uniform_numeric(df: pd.DataFrame, p_threshold: float = 0.01, min_columns: int = 2) -> CheckResult:
    """
    Continuous columns indistinguishable from a flat (uniform) distribution over their range
    (Kolmogorov-Smirnov p > `p_threshold`). Real measurements cluster and have tails.
    """
    name = "Flat numeric distributions"
    evidence = []
    for c in _continuous_columns(df):
        x = df[c].dropna()
        if len(x) < MIN_ROWS or x.max() == x.min():
            continue
        p = stats.kstest((x - x.min()) / (x.max() - x.min()), "uniform").pvalue
        if p > p_threshold:
            evidence.append({"column": c, "ks_p_value": round(float(p), 3)})
    triggered = len(evidence) >= min_columns
    return CheckResult(name, triggered, f"{len(evidence)} numeric column(s) indistinguishable from uniform", evidence)


# ---------------------------------------------------------------- 3. no relationships between columns

def no_relationships(df: pd.DataFrame, max_median_corr: float = 0.05, min_columns: int = 4) -> CheckResult:
    """
    Median absolute Pearson correlation across all pairs of continuous columns. Real variables recorded
    together almost always relate; independently generated ones sit near zero.
    """
    name = "No relationships between numeric columns"
    cols = _continuous_columns(df)
    if len(cols) < min_columns or len(df) < MIN_ROWS:
        return _not_applicable(name, f"needs {min_columns}+ continuous columns and {MIN_ROWS}+ rows")
    corr = df[cols].corr().abs().to_numpy()
    upper = corr[np.triu_indices(len(cols), 1)]
    median = float(np.nanmedian(upper))
    return CheckResult(name, median < max_median_corr,
                       f"median abs(r) = {median:.3f} across {len(upper)} column pairs",
                       [{"columns": len(cols), "pairs": int(len(upper)), "median_abs_corr": round(median, 4),
                         "max_abs_corr": round(float(np.nanmax(upper)), 4)}])


# ---------------------------------------------------------------- 4. place names that don't nest

_WORDS = r"[A-Za-z .'\-]+"


def inconsistent_hierarchy(df: pd.DataFrame, max_median_parents: float = 1.5) -> CheckResult:
    """
    For "Child, Parent" text columns (e.g. "Ghaziabad, Uttar Pradesh"), how many different parents each child
    appears with. A real city belongs to one state; random pairing puts it in many.
    """
    name = "Place names that don't nest"
    evidence, applicable = [], False
    for c in _text_columns(df):
        s = df[c].dropna().astype(str)
        if len(s) < MIN_ROWS:
            continue
        parts = s.str.split(", ")
        pairs = parts[parts.str.len() == 2]
        if len(pairs) / len(s) < 0.9:
            continue
        table = pd.DataFrame(pairs.tolist(), columns=["child", "parent"])
        if (table["child"].str.fullmatch(_WORDS).mean() < 0.9) or (table["parent"].str.fullmatch(_WORDS).mean() < 0.9):
            continue   # e.g. "September 25, 2021" is a date, not a place
        applicable = True
        median = float(table.groupby("child")["parent"].nunique().median())
        if median > max_median_parents:
            evidence.append({"column": c, "median_parents_per_child": median,
                             "children": int(table["child"].nunique()), "parents": int(table["parent"].nunique())})
    if not applicable:
        return _not_applicable(name, "no 'Child, Parent' text columns")
    return CheckResult(name, bool(evidence), f"{len(evidence)} place column(s) where names don't nest", evidence)


# ---------------------------------------------------------------- 5. placeholder text

LOREM_WORDS = frozenset("""
a ab accusamus accusantium ad adipisci alias aliquam aliquid amet animi aperiam architecto asperiores aspernatur
assumenda at atque aut autem beatae blanditiis commodi consectetur consequatur consequuntur corporis corrupti culpa
cum cumque cupiditate debitis delectus deleniti deserunt dicta dignissimos distinctio dolor dolore dolorem doloremque
dolores doloribus dolorum ducimus ea eaque earum eius eligendi enim eos error esse est et eum eveniet ex excepturi
exercitationem expedita explicabo facere facilis fuga fugiat fugit harum hic id illo illum impedit in incidunt
inventore ipsa ipsam ipsum iste itaque iure iusto labore laboriosam laborum laudantium libero magnam magni maiores
maxime minima minus modi molestiae molestias mollitia nam natus necessitatibus nemo neque nesciunt nihil nisi nobis
non nostrum nulla numquam obcaecati odio odit officia officiis omnis optio pariatur perferendis perspiciatis placeat
porro possimus praesentium provident quae quaerat quam quas quasi qui quia quibusdam quidem quis quisquam quo quod
quos ratione recusandae reiciendis rem repellat repellendus reprehenderit repudiandae rerum saepe sapiente sed sequi
similique sint sit soluta sunt suscipit tempora tempore temporibus tenetur totam ullam unde ut vel velit veniam
veritatis vero vitae voluptas voluptate voluptatem voluptates voluptatibus voluptatum
""".split())  # noqa: SIM905 - a word block reads better than a 182-item list


def placeholder_text(df: pd.DataFrame, min_share: float = 0.3, sample_rows: int = 300) -> CheckResult:
    """Free-text columns made mostly of lorem-ipsum vocabulary, the placeholder text used by fake-data tools."""
    name = "Placeholder (lorem ipsum) text"
    evidence, applicable = [], False
    for c in _text_columns(df):
        words = [re.sub(r"[^a-z]", "", w) for w in " ".join(df[c].dropna().astype(str).head(sample_rows)).lower().split()]
        words = [w for w in words if w]
        if len(words) < 200:
            continue
        applicable = True
        share = float(np.mean([w in LOREM_WORDS for w in words]))
        if share > min_share:
            evidence.append({"column": c, "lorem_word_share": round(share, 3)})
    if not applicable:
        return _not_applicable(name, "no free-text columns")
    return CheckResult(name, bool(evidence), f"{len(evidence)} text column(s) mostly lorem ipsum", evidence)


# ---------------------------------------------------------------- 6. identifiers that never repeat

_IP = re.compile(r"\d{1,3}(\.\d{1,3}){3}")


def never_repeating_identifiers(df: pd.DataFrame, min_rows: int = 1000) -> CheckResult:
    """
    IP-address columns where every value is unique. Real network logs are dominated by repeat sources and
    targets; per-row random generation never repeats.
    """
    name = "Network identifiers that never repeat"
    evidence, applicable = [], False
    for c in _text_columns(df):
        s = df[c].dropna().astype(str)
        if len(s) < min_rows or s.str.fullmatch(_IP.pattern).mean() < 0.9:
            continue
        applicable = True
        if s.is_unique:
            evidence.append({"column": c, "rows": int(len(s)), "unique": int(s.nunique())})
    if not applicable:
        return _not_applicable(name, "no IP-address columns")
    return CheckResult(name, bool(evidence), f"{len(evidence)} IP column(s) with no repeated value", evidence)


# ---------------------------------------------------------------- 7. Benford's law

BENFORD = np.log10(1 + 1 / np.arange(1, 10))


def first_digits(x: pd.Series) -> pd.Series:
    x = x[x > 0]
    return (x / 10 ** np.floor(np.log10(x))).astype(int).clip(1, 9)


def benford_deviation(df: pd.DataFrame, max_mad: float = 0.04, min_rows: int = 1000, min_orders: float = 1.9) -> CheckResult:
    """
    Mean absolute deviation (MAD) of leading-digit frequencies from Benford's law, for positive columns that
    span at least `min_orders` orders of magnitude (1st to 99th percentile), where Benford can apply at all.
    1.9 rather than 2: a range starting at zero spans exactly log10(99) = 1.996 between those percentiles.

    The 0.04 cutoff is deliberately loose: Nigrini's accounting threshold (0.015) flags real stock volumes in
    our calibration data, while uniformly generated columns sit near 0.06.
    """
    name = "Leading digits break Benford's law"
    evidence, applicable = [], False
    for c in _continuous_columns(df, min_distinct=20):
        x = df[c].dropna()
        x = x[x > 0]
        if len(x) < min_rows:
            continue
        span = np.log10(x.quantile(0.99) / x.quantile(0.01))
        if span < min_orders:
            continue
        applicable = True
        freq = first_digits(x).value_counts(normalize=True).reindex(range(1, 10), fill_value=0).to_numpy()
        mad = float(np.abs(freq - BENFORD).mean())
        if mad > max_mad:
            evidence.append({"column": c, "mad": round(mad, 4), "orders_of_magnitude": round(float(span), 1)})
    if not applicable:
        return _not_applicable(name, f"no positive column spans {min_orders:g}+ orders of magnitude")
    return CheckResult(name, bool(evidence), f"{len(evidence)} column(s) far from Benford's law", evidence)


# ---------------------------------------------------------------- 8. no daily rhythm

def flat_time_of_day(df: pd.DataFrame, p_threshold: float = 0.05, min_rows: int = 2000) -> CheckResult:
    """
    Timestamp columns whose events are spread evenly over the 24 hours (chi-square vs flat p > threshold).
    Human and machine activity both have daily cycles.
    """
    name = "No daily rhythm in timestamps"
    evidence, applicable = [], False
    for c in _text_columns(df):
        sample = df[c].dropna().astype(str).head(200)
        if sample.empty or sample.str.len().median() < 10 or not sample.str.contains(r"\d{1,2}:\d{2}").mean() > 0.9:
            continue
        times = pd.to_datetime(df[c], errors="coerce", format="mixed").dropna()
        if len(times) < min_rows or (times.dt.hour == 0).all():
            continue
        applicable = True
        counts = times.dt.hour.value_counts().reindex(range(24), fill_value=0).to_numpy()
        p = float(stats.chisquare(counts).pvalue)
        if p > p_threshold:
            evidence.append({"column": c, "chi2_p_value": round(p, 3), "min_hour_count": int(counts.min()),
                             "max_hour_count": int(counts.max())})
    if not applicable:
        return _not_applicable(name, "no timestamp columns with times of day")
    return CheckResult(name, bool(evidence), f"{len(evidence)} timestamp column(s) with no daily rhythm", evidence)


ALL_CHECKS = [even_categories, uniform_numeric, no_relationships, inconsistent_hierarchy, placeholder_text,
              never_repeating_identifiers, benford_deviation, flat_time_of_day]
