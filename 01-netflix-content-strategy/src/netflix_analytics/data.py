"""
Loading, cleaning and validating the Netflix titles snapshot.

The source is a 2021 snapshot of titles *available* on Netflix, with the date each was added. Known defects are
fixed and reported, never silently dropped; structural problems raise.
"""

from __future__ import annotations

from pathlib import Path

import numpy as np
import pandas as pd

REQUIRED_COLUMNS = ["show_id", "type", "title", "director", "cast", "country", "date_added", "release_year",
                    "rating", "duration", "listed_in", "description"]
TYPES = {"Movie", "TV Show"}

KIDS_RATINGS = {"TV-Y", "TV-Y7", "TV-Y7-FV", "TV-G", "G", "PG"}
MATURE_RATINGS = {"TV-MA", "R", "NC-17"}


class DataValidationError(ValueError):
    """The data breaks an assumption the analysis depends on."""


def load_titles(path: str | Path) -> tuple[pd.DataFrame, dict]:
    """Read, clean and validate the titles file. Returns the clean table and a data-quality report."""
    raw = pd.read_csv(path)
    missing = sorted(set(REQUIRED_COLUMNS) - set(raw.columns))
    if missing:
        raise DataValidationError(f"Missing columns: {missing}")
    df = raw.copy()

    # Dates have stray spaces; an explicit format makes unparseable values visible instead of guessed
    df["date_added"] = pd.to_datetime(df["date_added"].str.strip(), format="%B %d, %Y", errors="coerce")
    unparsed = df.loc[df["date_added"].isna(), ["show_id", "title"]]

    # A few rows have the duration in the rating column (a data-entry shift): move it back
    shifted = df["rating"].astype(str).str.fullmatch(r"\d+ min")
    shifted_rows = df.loc[shifted, ["show_id", "title", "rating"]].copy()
    df.loc[shifted, "duration"] = df.loc[shifted, "rating"]
    df.loc[shifted, "rating"] = np.nan

    df["year_added"] = df["date_added"].dt.year.astype("Int64")
    # For TV shows, release_year is the year of the *latest season*, not the first, so a show can appear to be
    # added before its "release". Content age is therefore only meaningful for movies.
    df["content_age"] = (df["year_added"] - df["release_year"]).where(df["type"] == "Movie")
    tv_release_after_added = df.loc[(df["type"] == "TV Show") & (df["year_added"] < df["release_year"]),
                                    ["title", "release_year", "year_added"]]
    df["is_tv"] = (df["type"] == "TV Show").astype(int)
    df["is_kids"] = df["rating"].isin(KIDS_RATINGS).astype(int)
    df["is_mature"] = df["rating"].isin(MATURE_RATINGS).astype(int)
    df["genres"] = df["listed_in"].str.split(", ")
    df["countries"] = df["country"].str.split(",").apply(
        lambda xs: [c.strip() for c in xs if c.strip()] if isinstance(xs, list) else [])

    validate_titles(df)
    report = {"titles": len(df), "unparsed_dates": unparsed.reset_index(drop=True),
              "rating_holds_duration": shifted_rows.reset_index(drop=True),
              "tv_release_year_after_added": tv_release_after_added.reset_index(drop=True),
              "last_date_added": df["date_added"].max().date()}
    return df, report


def validate_titles(df: pd.DataFrame) -> None:
    if df["show_id"].duplicated().any():
        raise DataValidationError("Duplicate show_id values")
    if not set(df["type"].dropna()) <= TYPES:
        raise DataValidationError(f"Unexpected types: {set(df['type']) - TYPES}")
    if (df["content_age"].dropna() < -1).any():
        raise DataValidationError("Movies added more than a year before their release year")
    if df["date_added"].dropna().gt(pd.Timestamp.now()).any():
        raise DataValidationError("Dates in the future")


def explode_list(df: pd.DataFrame, column: str, as_name: str) -> pd.DataFrame:
    """One row per title per list item (e.g. per country), so co-productions count for every country."""
    out = df.drop(columns=[as_name], errors="ignore").explode(column)
    return out.rename(columns={column: as_name}).dropna(subset=[as_name])


def window(df: pd.DataFrame, first: int, last: int) -> pd.DataFrame:
    return df[df["year_added"].between(first, last)].copy()
