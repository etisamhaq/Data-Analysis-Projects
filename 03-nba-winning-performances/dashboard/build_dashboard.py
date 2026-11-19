"""
Builds dashboard/data.json for the NBA win-probability page (dashboard/index.html).

    python dashboard/build_dashboard.py      (from the project folder, after download_data.py)

Uses the same package and model choices as the notebook: Elo with the adaptive home bonus (k=15, base bonus 80),
ratings as of the end of the last complete season in the data.
"""

import json
from pathlib import Path

import pandas as pd
from nba_analytics import data, elo, features, models
from nba_analytics import four_factors as ff

OUT = Path(__file__).resolve().parent / "data.json"
PARAMS = elo.EloParams(k=15, home_advantage=80, adaptive_home=True)


def main():
    games, _ = data.prepare_games("data/TeamStatistics.csv", first=2001, last=2024)
    home = data.home_view(games)
    ratings = elo.run_elo(home, PARAMS)
    last_season = int(home["season"].max())

    # Each team's rating after its final game of every season
    h = home.join(ratings)
    long = pd.concat([
        h[["date", "season", "teamId", "post_home"]].rename(columns={"post_home": "rating"}),
        h[["date", "season", "opponentTeamId", "post_away"]].rename(columns={"opponentTeamId": "teamId", "post_away": "rating"}),
    ]).sort_values("date")
    season_end = long.groupby(["teamId", "season"])["rating"].last().reset_index()

    names = (games.sort_values("date").groupby("teamId")[["teamCity", "teamName"]].last()
             .assign(name=lambda d: d["teamCity"] + " " + d["teamName"])["name"])
    final = season_end[season_end["season"] == last_season].assign(team=lambda d: d["teamId"].map(names))
    final = final.sort_values("rating", ascending=False)

    home_rate = home.groupby("season")["win"].mean()
    next_bonus = elo.home_bonus_from_rate(home_rate[home_rate.index <= last_season].tail(PARAMS.home_lookback).mean())

    home_ff = home.merge(ff.add_four_factors(games).query("home == 1")[["gameId"] + ["diff_" + f for f in ff.FACTORS]], on="gameId")
    win_rates = ff.factor_win_rates(home_ff).rename(ff.LABELS)
    weights = models.fit_factor_weights(home_ff)["relative_weight_pct"]

    pregame = features.pregame_features(home, ff.add_four_factors(games), ratings)
    split = models.time_split(pregame, train_last=2016, validation_last=2019)
    table, fitted = models.evaluate_predictors(pregame, features.FEATURES, split)
    test = table.loc["test"].reset_index()
    cal = models.calibration_table(fitted["parts"]["test"]["win"], fitted["predictions"]["Elo rating only"]["test"])

    data_out = {
        "as_of": data.season_label(last_season),
        "home_bonus": next_bonus,
        "teams": [{"team": t, "rating": round(r, 1)} for t, r in zip(final["team"], final["rating"], strict=True)],
        "rating_history": {names[tid]: [{"season": data.season_label(int(s)), "rating": round(r, 1)}
                                        for s, r in zip(g["season"], g["rating"], strict=True)]
                           for tid, g in season_end.groupby("teamId") if tid in names.index},
        "home_win_rate": [{"season": data.season_label(int(s)), "rate": float(v)} for s, v in home_rate.items()],
        "factor_win_rates": [{"factor": k, "win_rate": float(v)} for k, v in win_rates.items()],
        "factor_weights": [{"factor": k, "weight": float(v)} for k, v in weights.items()],
        "models": [{"model": r["model"], "accuracy": float(r["accuracy"]), "log_loss": float(r["log_loss"]),
                    "brier": float(r["brier"]), "games": int(r["games"])} for _, r in test.iterrows()],
        "calibration": [{"predicted": float(r["predicted"]), "observed": float(r["observed"]), "games": int(r["games"])}
                        for _, r in cal.iterrows()],
        "games": int(home["gameId"].nunique()),
    }
    OUT.write_text(json.dumps(data_out, separators=(",", ":")))
    print(f"Wrote {OUT} ({OUT.stat().st_size / 1024:.0f} KB); ratings as of {data_out['as_of']}, "
          f"next-season home bonus {next_bonus:.1f} Elo points")


if __name__ == "__main__":
    main()
