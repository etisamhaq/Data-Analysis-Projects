"""Small hand-built fixtures, so unit tests run in milliseconds and need no download."""

from pathlib import Path

import numpy as np
import pandas as pd
import pytest

DATA_FILE = Path(__file__).resolve().parents[1] / "data" / "TeamStatistics.csv"


def make_games(n_games: int = 40, n_teams: int = 4, seed: int = 0, season_start: str = "2015-10-27") -> pd.DataFrame:
    """A plausible season of team box scores: two rows per game, consistent scores and winners."""
    rng = np.random.default_rng(seed)
    rows = []
    start = pd.Timestamp(season_start, tz="UTC")
    for g in range(n_games):
        home, away = rng.choice(n_teams, size=2, replace=False)
        date = start + pd.Timedelta(days=g)
        scores = rng.integers(90, 125, size=2)
        if scores[0] == scores[1]:
            scores[0] += 1
        for team, opp, is_home, pts, opp_pts in [(home, away, 1, scores[0], scores[1]), (away, home, 0, scores[1], scores[0])]:
            fga = int(rng.integers(80, 95))
            fgm = int(rng.integers(35, 45))
            tpa = int(rng.integers(25, 40))
            rows.append({
                "gameId": 1000 + g, "gameDateTimeEst": date.strftime("%Y-%m-%d %H:%M:%S"), "date": date,
                "teamId": int(team), "opponentTeamId": int(opp), "teamCity": f"City{team}", "teamName": f"Team{team}",
                "home": is_home, "win": int(pts > opp_pts), "teamScore": int(pts), "opponentScore": int(opp_pts),
                "gameType": "Regular Season", "fieldGoalsMade": fgm, "fieldGoalsAttempted": fga,
                "threePointersMade": int(rng.integers(8, 15)), "threePointersAttempted": tpa,
                "freeThrowsMade": int(rng.integers(10, 20)), "freeThrowsAttempted": 22,
                "reboundsOffensive": int(rng.integers(6, 14)), "reboundsDefensive": int(rng.integers(30, 40)),
                "turnovers": int(rng.integers(8, 18)),
            })
    games = pd.DataFrame(rows)
    games["season"] = np.where(games["date"].dt.month >= 8, games["date"].dt.year, games["date"].dt.year - 1)
    return games


@pytest.fixture
def games() -> pd.DataFrame:
    return make_games()


@pytest.fixture
def real_data_path() -> Path:
    if not DATA_FILE.exists():
        pytest.skip("TeamStatistics.csv not downloaded (run `python download_data.py`)")
    return DATA_FILE
