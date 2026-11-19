"""Run every check on a table and turn the results into a verdict and a readable report."""

from __future__ import annotations

import json
from dataclasses import asdict, dataclass

import pandas as pd

from .checks import ALL_CHECKS, CheckResult

# Independent kinds of evidence needed for each verdict. One kind alone is common in real data
# (a balanced-by-design column, text in another language), so it only asks for a closer look.
LIKELY_SYNTHETIC = 2


@dataclass
class SanityReport:
    name: str
    rows: int
    columns: int
    results: list[CheckResult]

    @property
    def triggered(self) -> list[CheckResult]:
        return [r for r in self.results if r.triggered]

    @property
    def verdict(self) -> str:
        n = len(self.triggered)
        if n >= LIKELY_SYNTHETIC:
            return "LIKELY SYNTHETIC"
        if n == 1:
            return "INVESTIGATE"
        return "NO SYNTHETIC SIGNALS"

    @property
    def explanation(self) -> str:
        n = len(self.triggered)
        if n >= LIKELY_SYNTHETIC:
            return f"{n} independent kinds of evidence that rows were generated at random."
        if n == 1:
            return "One kind of evidence. Real data can trip a single check; look at the detail before trusting it."
        applicable = sum(r.applicable for r in self.results)
        return f"None of the {applicable} applicable checks found signs of random generation."

    def to_frame(self) -> pd.DataFrame:
        return pd.DataFrame([{"check": r.check, "applicable": r.applicable, "triggered": r.triggered,
                              "summary": r.summary} for r in self.results])

    def to_markdown(self) -> str:
        lines = [f"# Data sanity report: {self.name}", "",
                 f"**Verdict: {self.verdict}.** {self.explanation}", "",
                 f"{self.rows:,} rows x {self.columns} columns", "",
                 "| Check | Result | Detail |", "|---|---|---|"]
        for r in self.results:
            status = "TRIGGERED" if r.triggered else ("pass" if r.applicable else "n/a")
            lines.append(f"| {r.check} | {status} | {r.summary} |")
        for r in self.triggered:
            lines += ["", f"## {r.check}", ""]
            if r.evidence:
                keys = list(r.evidence[0])
                lines += ["| " + " | ".join(keys) + " |", "|" + "---|" * len(keys)]
                lines += ["| " + " | ".join(str(e.get(k, "")) for k in keys) + " |" for e in r.evidence]
        return "\n".join(lines) + "\n"

    def to_json(self) -> str:
        return json.dumps({"name": self.name, "rows": self.rows, "columns": self.columns, "verdict": self.verdict,
                           "explanation": self.explanation, "results": [asdict(r) for r in self.results]}, indent=2)


def check_dataframe(df: pd.DataFrame, name: str = "dataset") -> SanityReport:
    """Run all checks on `df`."""
    return SanityReport(name=name, rows=len(df), columns=df.shape[1], results=[check(df) for check in ALL_CHECKS])


def check_file(path: str, **read_kwargs) -> SanityReport:
    """Read a CSV (optionally compressed) or Excel file and run all checks."""
    if str(path).lower().endswith((".xlsx", ".xls")):
        df = pd.read_excel(path, **read_kwargs)
    else:
        df = pd.read_csv(path, low_memory=False, **read_kwargs)
    return check_dataframe(df, name=str(path).replace("\\", "/").split("/")[-1])
