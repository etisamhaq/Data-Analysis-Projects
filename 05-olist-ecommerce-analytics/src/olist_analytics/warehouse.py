"""
A small SQL warehouse in DuckDB, built the way analytics teams build them (the dbt pattern):

    raw_*     views over the source CSVs, untouched
    stg_*     sql/01_staging: one cleaned, typed model per source
    marts     sql/02_marts:   business-facing tables (files run in name order; the numeric prefix is dropped)
    tests     sql/03_tests:   each query returns the rows that break a rule; an empty result passes

Every model is a plain SELECT in its own file, so the SQL can be read, reviewed and run on its own.
"""

from __future__ import annotations

from dataclasses import dataclass
from pathlib import Path

import duckdb
import pandas as pd

PROJECT = Path(__file__).resolve().parents[2]
SQL_DIR = PROJECT / "sql"
DATA_DIR = PROJECT / "data"

RAW_FILES = {
    "raw_orders": "olist_orders_dataset.csv",
    "raw_customers": "olist_customers_dataset.csv",
    "raw_order_items": "olist_order_items_dataset.csv",
    "raw_payments": "olist_order_payments_dataset.csv",
    "raw_reviews": "olist_order_reviews_dataset.csv",
    "raw_products": "olist_products_dataset.csv",
    "raw_sellers": "olist_sellers_dataset.csv",
    "raw_geolocation": "olist_geolocation_dataset.csv",
    "raw_category_translation": "product_category_name_translation.csv",
}


class DataTestFailure(AssertionError):
    """One or more SQL data tests returned rows."""


@dataclass
class TestResult:
    name: str
    failing_rows: int


def model_name(path: Path) -> str:
    """'01_fct_orders.sql' -> 'fct_orders'."""
    stem = path.stem
    head, _, rest = stem.partition("_")
    return rest if head.isdigit() else stem


def load_raw(con: duckdb.DuckDBPyConnection, data_dir: Path = DATA_DIR) -> None:
    missing = [f for f in RAW_FILES.values() if not (data_dir / f).exists()]
    if missing:
        raise FileNotFoundError(f"Missing source files in {data_dir}: {missing}. Run `python download_data.py`.")
    for view, filename in RAW_FILES.items():
        path = (data_dir / filename).as_posix()
        con.execute(f"create or replace view {view} as select * from read_csv_auto('{path}', header = true)")


def run_models(con: duckdb.DuckDBPyConnection, sql_dir: Path = SQL_DIR) -> list[str]:
    built = []
    for layer in ("01_staging", "02_marts"):
        for path in sorted((sql_dir / layer).glob("*.sql")):
            name = model_name(path)
            con.execute(f"create or replace table {name} as\n{path.read_text()}")
            built.append(name)
    return built


def run_tests(con: duckdb.DuckDBPyConnection, sql_dir: Path = SQL_DIR, raise_on_failure: bool = True) -> list[TestResult]:
    results = []
    for path in sorted((sql_dir / "03_tests").glob("*.sql")):
        n = con.execute(f"select count(*) from ({path.read_text()})").fetchone()[0]
        results.append(TestResult(path.stem, n))
    failures = [r for r in results if r.failing_rows]
    if failures and raise_on_failure:
        detail = ", ".join(f"{r.name} ({r.failing_rows} rows)" for r in failures)
        raise DataTestFailure(f"{len(failures)} data test(s) failed: {detail}")
    return results


def build(database: str | Path = ":memory:", data_dir: Path = DATA_DIR, sql_dir: Path = SQL_DIR) -> duckdb.DuckDBPyConnection:
    """Load sources, build every model, run every data test; returns the open connection."""
    con = duckdb.connect(str(database))
    load_raw(con, data_dir)
    run_models(con, sql_dir)
    run_tests(con, sql_dir)
    return con


def query(con: duckdb.DuckDBPyConnection, sql: str) -> pd.DataFrame:
    return con.execute(sql).fetchdf()
