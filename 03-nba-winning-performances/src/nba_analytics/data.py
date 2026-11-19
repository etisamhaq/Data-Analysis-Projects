"""
Loading and validating team box scores.

The source (Kaggle, "Historical NBA Data and Player Box Scores") has one row per team per game. Everything
downstream assumes two rows per game, consistent scores and complete seasons, so those are checked here and
violations raise instead of silently producing wrong numbers.
"""

from __future__ import annotations

from pathlib import Path

import numpy as np
import pandas as pd

REQUIRED_COLUMNS = [
    "gameId", "gameDateTimeEst", "teamId", "opponentTeamId", "teamCity", "teamName", "home", "win",
    "teamScore", "opponentScore", "gameType", "fieldGoalsMade", "fieldGoalsAttempted", "threePointersMade",
    "threePointersAttempted", "freeThrowsMade", "freeThrowsAttempted", "reboundsOffensive",
    "reboundsDefensive", "turnovers",
]

# A full season has 1,189-1,230 games; the shortest real ones (COVID 2019-20, lockout 2011-12) have
# 979-990 here. Far fewer means games are missing from the source, not a short season.
MIN_GAMES_PER_SEASON = 900


class DataValidationError(ValueError):
    """The data breaks an assumption the analysis depends on."""


def season_of(game_ids: pd.Series) -> pd.Series:
    """
    NBA season label from the league's game ID: 22400627 = regular season (2), season 24 (2024-25), game 627.

    The ID is authoritative. Inferring the season from the date fails for the 2020 "bubble": its games ran into
    August 2020 but belong to 2019-20, and a month-based rule assigned 80 of them to 2020-21.
    """
    yy = game_ids.astype(int).astype(str).str[-7:-5].astype(int)
    return pd.Series(np.where(yy >= 46, 1900 + yy, 2000 + yy), index=game_ids.index)


def season_label(season: int) -> str:
    return f"{season}-{str(season + 1)[2:]}"


def load_team_games(path: str | Path, game_type: str = "Regular Season") -> pd.DataFrame:
    """Read TeamStatistics.csv, keep one game type and add `date` and `season`. Not yet validated."""
    raw = pd.read_csv(path, low_memory=False)
    missing = sorted(set(REQUIRED_COLUMNS) - set(raw.columns))
    if missing:
        raise DataValidationError(f"Missing columns: {missing}")
    games = raw.loc[raw["gameType"] == game_type, REQUIRED_COLUMNS].copy()
    games["date"] = pd.to_datetime(games["gameDateTimeEst"], utc=True)
    games["season"] = season_of(games["gameId"])
    games["win"] = games["win"].astype(int)
    return games.sort_values(["date", "gameId", "home"]).reset_index(drop=True)


def inconsistent_games(games: pd.DataFrame) -> pd.DataFrame:
    """
    Games whose recorded result can't be trusted: a tied final score (impossible in the NBA, so overtime
    points are missing), or a `win` flag that disagrees with the score.
    """
    tied = games["teamScore"] == games["opponentScore"]
    disagrees = np.where(games["win"] == 1, games["teamScore"] < games["opponentScore"],
                         games["teamScore"] > games["opponentScore"])
    bad_ids = games.loc[tied | disagrees, "gameId"].unique()
    bad = games[games["gameId"].isin(bad_ids) & (games["home"] == 1)]
    reason = np.where(bad["teamScore"] == 0, "score recorded as 0-0", "tied final score (overtime points missing)")
    return pd.DataFrame({"gameId": bad["gameId"], "date": bad["date"].dt.date,
                         "matchup": bad["teamName"] + " (home)", "reason": reason}).reset_index(drop=True)


def prepare_games(path: str | Path, first: int, last: int,
                  min_games: int = MIN_GAMES_PER_SEASON) -> tuple[pd.DataFrame, dict]:
    """
    Load, keep complete seasons first..last, drop games with untrustworthy results, then validate strictly.

    Returns the clean games and a data-quality report describing everything that was removed.
    """
    games = load_team_games(path)
    window = games[games["season"].between(first, last)]
    completeness = season_completeness(window, min_games)
    kept = window[window["season"].isin(completeness.index[completeness["complete"]])]
    bad = inconsistent_games(kept)
    clean = kept[~kept["gameId"].isin(bad["gameId"])].copy()
    validate_team_games(clean)
    report = {
        "seasons_kept": [season_label(s) for s in completeness.index[completeness["complete"]]],
        "seasons_dropped": {season_label(s): int(n) for s, n in
                            completeness.loc[~completeness["complete"], "games"].items()},
        "games_dropped_inconsistent": bad,
        "games": int(clean["gameId"].nunique()),
    }
    return clean.reset_index(drop=True), report


def validate_team_games(games: pd.DataFrame) -> None:
    """Raise DataValidationError if the box scores are internally inconsistent."""
    rows_per_game = games.groupby("gameId").size()
    if (rows_per_game != 2).any():
        bad = rows_per_game[rows_per_game != 2]
        raise DataValidationError(f"{len(bad)} games do not have exactly two team rows, e.g. {bad.index[:3].tolist()}")
    if games[["teamScore", "opponentScore", "fieldGoalsAttempted"]].isna().any().any():
        raise DataValidationError("Missing scores or shot attempts")
    if not games.groupby("gameId")["win"].sum().eq(1).all():
        raise DataValidationError("Some games do not have exactly one winner")
    winner_scored_more = np.where(games["win"] == 1, games["teamScore"] > games["opponentScore"],
                                  games["teamScore"] < games["opponentScore"])
    if not winner_scored_more.all():
        raise DataValidationError("`win` disagrees with the score in some rows")
    impossible = (games["fieldGoalsMade"] > games["fieldGoalsAttempted"]) | (
        games["threePointersMade"] > games["threePointersAttempted"]) | (
        games["freeThrowsMade"] > games["freeThrowsAttempted"])
    if impossible.any():
        raise DataValidationError(f"{impossible.sum()} rows have more shots made than attempted")


def season_completeness(games: pd.DataFrame, min_games: int = MIN_GAMES_PER_SEASON) -> pd.DataFrame:
    """Games per season and whether each season is complete enough to analyse."""
    per_season = games.groupby("season")["gameId"].nunique().rename("games").to_frame()
    per_season["complete"] = per_season["games"] >= min_games
    per_season.index = per_season.index.map(int)
    return per_season


def home_view(games: pd.DataFrame) -> pd.DataFrame:
    """One row per game from the home team's point of view, in date order."""
    home = games[games["home"] == 1].sort_values(["date", "gameId"]).reset_index(drop=True)
    if home["gameId"].duplicated().any():
        raise DataValidationError("A game has two home teams")
    return home
