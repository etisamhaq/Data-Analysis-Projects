"""
Elo ratings for NBA teams, producing a pre-game home-win probability for every game.

The design follows the widely used FiveThirtyEight NBA Elo:
- every team starts at 1500;
- the home team gets a fixed rating bonus;
- after each game the winner takes points from the loser, scaled by K and by a margin-of-victory multiplier
  that is damped when the favourite wins (so blowouts by strong teams don't inflate ratings);
- between seasons each rating moves part of the way back to the mean, because rosters change;
- optionally (`adaptive_home=True`) the home bonus is re-estimated each season from the home win rate of the
  previous seasons, because NBA home advantage has shrunk (about 60% home wins in the 2000s, 54-55% in
  2019-24) and a fixed bonus makes the model overconfident about home teams.

Probabilities are recorded *before* each game's result updates the ratings, so they use only past information.
"""

from __future__ import annotations

from dataclasses import dataclass

import numpy as np
import pandas as pd


@dataclass(frozen=True)
class EloParams:
    k: float = 20.0
    home_advantage: float = 100.0
    season_regression: float = 0.25
    initial: float = 1500.0
    adaptive_home: bool = False
    home_lookback: int = 3


def home_bonus_from_rate(home_win_rate: float) -> float:
    """Elo points that make two equal teams' home win probability equal `home_win_rate`."""
    return 400.0 * np.log10(home_win_rate / (1.0 - home_win_rate))


def season_home_bonus(home_games: pd.DataFrame, params: EloParams) -> dict[int, float]:
    """Home bonus for each season: fixed, or from the previous `home_lookback` seasons' home win rate."""
    seasons = sorted(home_games["season"].unique())
    if not params.adaptive_home:
        return {s: params.home_advantage for s in seasons}
    rate = home_games.groupby("season")["win"].mean()
    bonus = {}
    for s in seasons:
        past = rate[rate.index < s].tail(params.home_lookback)
        bonus[s] = home_bonus_from_rate(past.mean()) if len(past) else params.home_advantage
    return bonus


def expected_score(rating_a: float, rating_b: float) -> float:
    """Probability that A beats B under the logistic Elo model."""
    return 1.0 / (1.0 + 10 ** ((rating_b - rating_a) / 400.0))


def margin_multiplier(margin: float, winner_elo_edge: float) -> float:
    """Larger wins move ratings more, but less so when the favourite was expected to win big."""
    return np.log(abs(margin) + 1) * 2.2 / (winner_elo_edge * 0.001 + 2.2)


def run_elo(home_games: pd.DataFrame, params: EloParams | None = None) -> pd.DataFrame:
    """
    Walk through games in date order and return pre-game ratings and the home-win probability.

    `home_games`: one row per game (home team's view) with date, season, teamId, opponentTeamId, win,
    teamScore, opponentScore.
    """
    params = params or EloParams()
    games = home_games.sort_values(["date", "gameId"])
    bonus = season_home_bonus(games, params)
    ratings: dict[int, float] = {}
    last_season: dict[int, int] = {}
    out = np.empty((len(games), 3))

    for i, g in enumerate(games.itertuples(index=False)):
        for team in (g.teamId, g.opponentTeamId):
            if team not in ratings:
                ratings[team] = params.initial
            elif last_season[team] != g.season:
                ratings[team] = ratings[team] * (1 - params.season_regression) + params.initial * params.season_regression
            last_season[team] = g.season

        home_elo, away_elo = ratings[g.teamId], ratings[g.opponentTeamId]
        hfa = bonus[g.season]
        p_home = expected_score(home_elo + hfa, away_elo)
        out[i] = (home_elo, away_elo, p_home)

        edge = (home_elo + hfa - away_elo) * (1 if g.win == 1 else -1)
        shift = params.k * margin_multiplier(g.teamScore - g.opponentScore, edge) * (g.win - p_home)
        ratings[g.teamId] += shift
        ratings[g.opponentTeamId] -= shift

    result = pd.DataFrame(out, columns=["elo_home", "elo_away", "p_home_elo"], index=games.index)
    return result.reindex(home_games.index)


def tune_elo(home_games: pd.DataFrame, seasons: tuple[int, int], grid: dict[str, list]) -> tuple[EloParams, pd.DataFrame]:
    """
    Grid-search Elo parameters by log loss on `seasons` (inclusive). Pass training seasons only, so the
    evaluation seasons stay untouched. Ratings still run over all earlier games for warm-up.
    """
    from itertools import product

    from sklearn.metrics import log_loss

    mask = home_games["season"].between(*seasons)
    rows = []
    for values in product(*grid.values()):
        params = EloParams(**dict(zip(grid.keys(), values, strict=True)))
        p = run_elo(home_games, params)["p_home_elo"]
        rows.append({**dict(zip(grid.keys(), values, strict=True)), "log_loss": log_loss(home_games.loc[mask, "win"], p[mask])})
    table = pd.DataFrame(rows).sort_values("log_loss").reset_index(drop=True)
    best = {k: table.loc[0, k] for k in grid}
    best = {k: (bool(v) if isinstance(grid[k][0], bool) else float(v)) for k, v in best.items()}
    return EloParams(**best), table
