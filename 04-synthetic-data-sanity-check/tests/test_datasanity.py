"""
Unit tests build small tables that should and shouldn't trip each check. Integration tests run the full
report on the two synthetic datasets (must be flagged) and on three real ones (must not be): the real
datasets are the false-positive control.
"""

from pathlib import Path

import numpy as np
import pandas as pd
import pytest
from datasanity import check_dataframe, check_file, checks
from datasanity.cli import main

ROOT = Path(__file__).resolve().parents[2]
SYNTHETIC = [ROOT / "04-synthetic-data-sanity-check/data/cybersecurity_attacks.csv.gz",
             ROOT / "04-synthetic-data-sanity-check/data/air_quality_health_impact_data.csv.gz"]
REAL = [ROOT / "01-netflix-content-strategy/netflix_titles.csv",
        ROOT / "02-adani-ports-stock-analysis/ADANIPORTS.csv"]
NBA = ROOT / "03-nba-winning-performances/data/TeamStatistics.csv"

rng = np.random.default_rng(0)
N = 3000


def realistic_frame() -> pd.DataFrame:
    """Skewed, related, clustered: what recorded data usually looks like."""
    size = rng.lognormal(3, 1, N)
    return pd.DataFrame({
        "size": size,
        "cost": size * rng.normal(2.0, 0.2, N),
        "weight": size ** 0.8 + rng.normal(0, 1, N),
        "rating": np.clip(rng.normal(70, 10, N), 0, 100),
        "segment": rng.choice(["retail", "wholesale", "online"], N, p=[0.6, 0.3, 0.1]),
        "region": rng.choice(["north", "south", "east", "west"], N, p=[0.4, 0.3, 0.2, 0.1]),
    })


def random_frame() -> pd.DataFrame:
    """Independent uniform draws: what fake-data generators produce."""
    return pd.DataFrame({
        "a": rng.uniform(0, 500, N), "b": rng.uniform(0, 200, N), "c": rng.uniform(-10, 40, N),
        "d": rng.uniform(0, 100, N),
        "kind": rng.choice(["x", "y", "z"], N), "level": rng.choice(["low", "mid", "high"], N),
    })


# ---------------------------------------------------------------- individual checks

def test_even_categories_flags_random_not_skewed():
    assert checks.even_categories(random_frame()).triggered
    assert not checks.even_categories(realistic_frame()).triggered


def test_a_single_balanced_column_is_not_enough():
    df = realistic_frame().assign(side=np.tile(["home", "away", "neutral"], N // 3))
    result = checks.even_categories(df)
    assert len(result.evidence) == 1 and not result.triggered


def test_uniform_numeric():
    assert checks.uniform_numeric(random_frame()).triggered
    assert not checks.uniform_numeric(realistic_frame()).triggered


def test_row_ids_are_not_mistaken_for_uniform_data():
    df = realistic_frame().assign(row_id=np.arange(N))
    assert all(e["column"] != "row_id" for e in checks.uniform_numeric(df).evidence)


def test_no_relationships():
    assert checks.no_relationships(random_frame()).triggered
    assert not checks.no_relationships(realistic_frame()).triggered


def test_no_relationships_needs_enough_columns():
    assert not checks.no_relationships(random_frame()[["a", "b"]]).applicable


def test_hierarchy_flags_random_pairs_and_ignores_dates():
    cities = [f"City{chr(65 + i)}" for i in range(20)]
    states = [f"State{chr(65 + i)}" for i in range(5)]
    home_state = dict(zip(cities, rng.choice(states, len(cities)), strict=True))
    child = rng.choice(cities, N)
    real = pd.DataFrame({"place": [f"{c}, {home_state[c]}" for c in child]})
    fake = pd.DataFrame({"place": [f"{c}, {s}" for c, s in zip(child, rng.choice(states, N), strict=True)]})
    dates = pd.DataFrame({"added": rng.choice(["September 25, 2021", "May 1, 2019"], N)})
    assert checks.inconsistent_hierarchy(fake).triggered
    assert not checks.inconsistent_hierarchy(real).triggered
    assert not checks.inconsistent_hierarchy(dates).applicable


def test_placeholder_text():
    lorem = pd.DataFrame({"t": ["Qui natus odio asperiores nam. Optio nobis iusto accusamus ad perferendis esse at."] * 50})
    english = pd.DataFrame({"t": ["A young chef moves to the city and opens a small restaurant with her friends."] * 50})
    assert checks.placeholder_text(lorem).triggered
    assert not checks.placeholder_text(english).triggered


def test_never_repeating_identifiers():
    unique = pd.DataFrame({"ip": [f"10.{i // 65536 % 256}.{i // 256 % 256}.{i % 256}" for i in range(2000)]})
    repeated = pd.DataFrame({"ip": rng.choice(unique["ip"].head(50), 2000)})
    assert checks.never_repeating_identifiers(unique).triggered
    assert not checks.never_repeating_identifiers(repeated).triggered


def test_benford():
    benford_like = pd.DataFrame({"amount": rng.lognormal(5, 2.5, 5000)})
    uniform = pd.DataFrame({"amount": rng.uniform(0, 500, 5000)})
    narrow = pd.DataFrame({"amount": rng.uniform(90, 130, 5000)})
    assert not checks.benford_deviation(benford_like).triggered
    assert checks.benford_deviation(uniform).triggered
    assert not checks.benford_deviation(narrow).applicable   # Benford can't apply to a narrow range


def test_first_digits():
    assert checks.first_digits(pd.Series([0.0123, 4.5, 987.0, 20.0])).tolist() == [1, 4, 9, 2]


def test_flat_time_of_day():
    start = pd.Timestamp("2023-01-01")
    flat = pd.DataFrame({"ts": (start + pd.to_timedelta(rng.uniform(0, 365 * 86400, 5000), unit="s")).astype(str)})
    hours = rng.choice(range(24), 5000, p=np.r_[np.full(8, 0.005), np.full(16, (1 - 0.04) / 16)])
    rhythmic = pd.DataFrame({"ts": (start + pd.to_timedelta(rng.integers(0, 365, 5000), unit="D")
                                    + pd.to_timedelta(hours, unit="h")).astype(str)})
    assert checks.flat_time_of_day(flat).triggered
    assert not checks.flat_time_of_day(rhythmic).triggered


# ---------------------------------------------------------------- verdict

def test_verdicts():
    assert check_dataframe(random_frame()).verdict == "LIKELY SYNTHETIC"
    assert check_dataframe(realistic_frame()).verdict == "NO SYNTHETIC SIGNALS"


def test_markdown_tables_are_not_broken_by_cell_text():
    markdown = check_dataframe(random_frame()).to_markdown()
    main_table = markdown.split("| Check | Result | Detail |")[1].split("\n## ")[0]
    rows = [line for line in main_table.splitlines() if line.startswith("| ")]
    assert len(rows) == len(checks.ALL_CHECKS)
    assert all(line.count("|") == 4 for line in rows), rows


# ---------------------------------------------------------------- real files: synthetic flagged, real data clean

@pytest.mark.data
@pytest.mark.parametrize("path", SYNTHETIC, ids=lambda p: p.name)
def test_known_synthetic_datasets_are_flagged(path):
    assert check_file(path).verdict == "LIKELY SYNTHETIC"


@pytest.mark.data
@pytest.mark.parametrize("path", REAL, ids=lambda p: p.name)
def test_real_datasets_are_not_flagged(path):
    report = check_file(path)
    assert report.verdict == "NO SYNTHETIC SIGNALS", report.to_markdown()


@pytest.mark.data
def test_real_nba_data_is_not_flagged():
    if not NBA.exists():
        pytest.skip("NBA data not downloaded")
    assert check_file(NBA).verdict == "NO SYNTHETIC SIGNALS"


def test_cli_exit_code(tmp_path):
    fake, real = tmp_path / "fake.csv", tmp_path / "real.csv"
    random_frame().to_csv(fake, index=False)
    realistic_frame().to_csv(real, index=False)
    assert main([str(real), "--fail-on-synthetic", "--out", str(tmp_path / "out")]) == 0
    assert main([str(fake), "--fail-on-synthetic", "--out", str(tmp_path / "out")]) == 1
    assert (tmp_path / "out" / "fake_sanity.md").read_text().startswith("# Data sanity report")
