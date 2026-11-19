"""
Dean Oliver's Four Factors, computed for both teams in every game.

| Factor | Formula |
|---|---|
| eFG%     | (FGM + 0.5 * 3PM) / FGA |
| TOV%     | TOV / (FGA + 0.44 * FTA + TOV) |
| ORB%     | ORB / (ORB + opponent DRB) |
| FT rate  | FTM / FGA |
"""

from __future__ import annotations

import pandas as pd

FACTORS = ["efg", "tov", "orb", "ftr"]
LABELS = {
    "efg": "Shooting (eFG%)",
    "tov": "Turnovers (TOV%)",
    "orb": "Off. rebounding (ORB%)",
    "ftr": "Free throws (FT rate)",
}
# Oliver's published weights, for comparison
OLIVER_WEIGHTS = {"efg": 40, "tov": 25, "orb": 20, "ftr": 15}

_BOX = ["fieldGoalsMade", "fieldGoalsAttempted", "threePointersMade", "turnovers", "freeThrowsAttempted",
        "freeThrowsMade", "reboundsOffensive", "reboundsDefensive"]


def effective_fg(fgm, fga, tpm):
    return (fgm + 0.5 * tpm) / fga


def turnover_rate(tov, fga, fta):
    return tov / (fga + 0.44 * fta + tov)


def offensive_rebound_rate(orb, opp_drb):
    return orb / (orb + opp_drb)


def free_throw_rate(ftm, fga):
    return ftm / fga


def with_opponent(games: pd.DataFrame) -> pd.DataFrame:
    """Attach each row's opponent box score as *_opp columns (each game has exactly two rows)."""
    opp = games[["gameId", "teamId", *_BOX]]
    both = games.merge(opp, on="gameId", suffixes=("", "_opp"))
    return both[both["teamId"] != both["teamId_opp"]].reset_index(drop=True)


def add_four_factors(games: pd.DataFrame) -> pd.DataFrame:
    """Each team's and its opponent's Four Factors, plus their gaps (`diff_*`, team minus opponent)."""
    d = with_opponent(games)
    for side, s, o in [("", "", "_opp"), ("_opp", "_opp", "")]:
        d["efg" + side] = effective_fg(d["fieldGoalsMade" + s], d["fieldGoalsAttempted" + s], d["threePointersMade" + s])
        d["tov" + side] = turnover_rate(d["turnovers" + s], d["fieldGoalsAttempted" + s], d["freeThrowsAttempted" + s])
        d["orb" + side] = offensive_rebound_rate(d["reboundsOffensive" + s], d["reboundsDefensive" + o])
        d["ftr" + side] = free_throw_rate(d["freeThrowsMade" + s], d["fieldGoalsAttempted" + s])
    for f in FACTORS:
        d["diff_" + f] = d[f] - d[f + "_opp"]
    return d


def factor_win_rates(home: pd.DataFrame) -> pd.Series:
    """
    Share of games won by whichever team was better at each factor (fewer turnovers is better).

    `home` has one row per game with `diff_*` columns from the home team's view. Games where both teams are
    exactly equal on a factor have no "better" team and are left out of that factor's rate.
    """
    rates = {}
    for f in FACTORS:
        diff = home["diff_" + f]
        decided = diff != 0
        home_better = (diff < 0) if f == "tov" else (diff > 0)
        better_team_won = (home_better & (home["win"] == 1)) | (~home_better & (home["win"] == 0))
        rates[f] = better_team_won[decided].mean() * 100
    return pd.Series(rates)
