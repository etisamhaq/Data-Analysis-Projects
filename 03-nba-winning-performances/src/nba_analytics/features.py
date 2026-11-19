"""
Pre-game features: what a model is allowed to know before tip-off.

Every feature for a game is built only from games that finished earlier. The core tool is a per-team rolling
mean that is shifted by one game, so a team's form going into game n uses games n-k..n-1 and never game n.
"""

from __future__ import annotations

import pandas as pd

from .four_factors import FACTORS


def team_form(team_games: pd.DataFrame, window: int = 10) -> pd.DataFrame:
    """
    Each team's rolling Four Factors going *into* every game, plus rest days.

    `team_games`: one row per team per game with date, season, teamId and the factor columns from
    `four_factors.add_four_factors` (own: efg, tov, ...; allowed: efg_opp, tov_opp, ...).
    Form resets at the start of each season; early-season games use however many games exist (min 3).
    """
    d = team_games.sort_values(["teamId", "date", "gameId"]).copy()
    cols = FACTORS + [f + "_opp" for f in FACTORS]
    grouped = d.groupby(["teamId", "season"], sort=False)[cols]
    form = grouped.transform(lambda s: s.shift(1).rolling(window, min_periods=3).mean())
    form.columns = ["form_" + c for c in cols]
    d = pd.concat([d, form], axis=1)

    previous_game = d.groupby("teamId")["date"].shift(1)
    d["rest_days"] = (d["date"] - previous_game).dt.days.clip(upper=7)
    d["back_to_back"] = (d["rest_days"] == 1).astype(int)
    keep = ["gameId", "teamId", "rest_days", "back_to_back", *form.columns]
    return d[keep]


def pregame_features(home: pd.DataFrame, team_games: pd.DataFrame, elo: pd.DataFrame, window: int = 10) -> pd.DataFrame:
    """
    One row per game: home-minus-away gaps in Elo, recent offensive and defensive form, and rest.

    `home`: one row per game (home view). `elo`: output of `elo.run_elo` aligned to `home`.
    """
    form = team_form(team_games, window)
    h = home[["gameId", "date", "season", "teamId", "opponentTeamId", "win"]].copy()
    h = h.merge(form, on=["gameId", "teamId"], how="left")
    away = form.rename(columns={"teamId": "opponentTeamId"})
    h = h.merge(away, on=["gameId", "opponentTeamId"], how="left", suffixes=("", "_away"))

    out = h[["gameId", "date", "season", "win"]].copy()
    out["elo_diff"] = (elo["elo_home"] - elo["elo_away"]).to_numpy()
    out["p_home_elo"] = elo["p_home_elo"].to_numpy()
    for f in FACTORS:
        # Offence: how well the home team has been doing this, vs the away team
        out[f"off_{f}_gap"] = h[f"form_{f}"] - h[f"form_{f}_away"]
        # Defence: what the home team has been allowing, vs the away team (lower allowed is better)
        out[f"def_{f}_gap"] = h[f"form_{f}_opp"] - h[f"form_{f}_opp_away"]
    out["rest_gap"] = h["rest_days"] - h["rest_days_away"]
    out["home_b2b"] = h["back_to_back"]
    out["away_b2b"] = h["back_to_back_away"]
    return out


FEATURES = (["elo_diff"] + [f"off_{f}_gap" for f in FACTORS] + [f"def_{f}_gap" for f in FACTORS]
            + ["rest_gap", "home_b2b", "away_b2b"])
