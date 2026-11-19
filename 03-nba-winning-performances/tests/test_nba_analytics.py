import numpy as np
import pandas as pd
import pytest
from nba_analytics import data, elo, features, models
from nba_analytics import four_factors as ff

# ---------------------------------------------------------------- four factors


def test_factor_formulas_match_hand_calculation():
    assert ff.effective_fg(40, 88, 12) == pytest.approx((40 + 6) / 88)
    assert ff.turnover_rate(14, 88, 20) == pytest.approx(14 / (88 + 8.8 + 14))
    assert ff.offensive_rebound_rate(10, 30) == pytest.approx(0.25)
    assert ff.free_throw_rate(18, 90) == pytest.approx(0.2)


def test_factor_gaps_are_antisymmetric(games):
    d = ff.add_four_factors(games)
    pair = d.sort_values(["gameId", "home"]).groupby("gameId")
    for f in ff.FACTORS:
        assert pair["diff_" + f].sum().abs().max() == pytest.approx(0, abs=1e-12)


def test_win_rate_ignores_ties():
    home = pd.DataFrame({"diff_efg": [0.1, -0.1, 0.0], "diff_tov": [-0.1, 0.1, 0.0],
                         "diff_orb": [0.1, 0.1, 0.1], "diff_ftr": [0.1, 0.1, 0.1], "win": [1, 0, 1]})
    rates = ff.factor_win_rates(home)
    assert rates["efg"] == pytest.approx(100.0)   # tie in game 3 excluded; better team won games 1 and 2
    assert rates["tov"] == pytest.approx(100.0)   # fewer turnovers counts as better


# ---------------------------------------------------------------- data validation


def test_validation_accepts_clean_data(games):
    data.validate_team_games(games)


def test_validation_rejects_missing_team_row(games):
    with pytest.raises(data.DataValidationError, match="exactly two"):
        data.validate_team_games(games.iloc[1:])


def test_validation_rejects_made_above_attempted(games):
    broken = games.copy()
    broken.loc[0, "fieldGoalsMade"] = broken.loc[0, "fieldGoalsAttempted"] + 1
    with pytest.raises(data.DataValidationError, match="made than attempted"):
        data.validate_team_games(broken)


def test_inconsistent_games_flags_ties_and_zero_scores(games):
    broken = games.copy()
    first, second = broken["gameId"].unique()[:2]
    broken.loc[broken["gameId"] == first, ["teamScore", "opponentScore"]] = 100
    broken.loc[broken["gameId"] == second, ["teamScore", "opponentScore"]] = 0
    flagged = data.inconsistent_games(broken).set_index("gameId")["reason"]
    assert set(flagged.index) == {first, second}
    assert "tied" in flagged[first] and "0-0" in flagged[second]


def test_season_labels():
    dates = pd.Series(pd.to_datetime(["2024-10-22", "2025-04-13", "2025-08-01"]))
    assert data.season_of(dates).tolist() == [2024, 2024, 2025]
    assert data.season_label(2024) == "2024-25"


# ---------------------------------------------------------------- Elo


def test_expected_score_is_symmetric():
    assert elo.expected_score(1500, 1500) == pytest.approx(0.5)
    assert elo.expected_score(1600, 1500) + elo.expected_score(1500, 1600) == pytest.approx(1.0)


def test_home_bonus_round_trips_to_win_rate():
    bonus = elo.home_bonus_from_rate(0.58)
    assert elo.expected_score(1500 + bonus, 1500) == pytest.approx(0.58)
    assert elo.home_bonus_from_rate(0.5) == pytest.approx(0.0)


def test_elo_is_zero_sum_and_pregame(games):
    home = data.home_view(games)
    before = elo.run_elo(home, elo.EloParams(home_advantage=0))
    assert ((before["elo_home"] + before["elo_away"]) > 0).all()

    # Flipping a game's result must not change that game's own pre-game probability...
    flipped = home.copy()
    i = 10
    flipped.loc[i, ["teamScore", "opponentScore"]] = flipped.loc[i, ["opponentScore", "teamScore"]].to_numpy()
    flipped.loc[i, "win"] = 1 - flipped.loc[i, "win"]
    after = elo.run_elo(flipped, elo.EloParams(home_advantage=0))
    assert after.loc[i, "p_home_elo"] == pytest.approx(before.loc[i, "p_home_elo"])
    # ...but it must change later games involving those teams
    assert not np.allclose(after["p_home_elo"].iloc[i + 1:], before["p_home_elo"].iloc[i + 1:])


def test_adaptive_home_bonus_uses_only_past_seasons(games):
    two = pd.concat([games, games.assign(gameId=games["gameId"] + 10_000,
                                         date=games["date"] + pd.Timedelta(days=365), season=games["season"] + 1)])
    home = data.home_view(two)
    params = elo.EloParams(home_advantage=77, adaptive_home=True)
    bonus = elo.season_home_bonus(home, params)
    first, second = sorted(bonus)
    assert bonus[first] == 77   # no past seasons: falls back to the fixed bonus
    assert bonus[second] == pytest.approx(elo.home_bonus_from_rate(home.loc[home["season"] == first, "win"].mean()))


# ---------------------------------------------------------------- pre-game features: no leakage


def test_form_features_never_see_the_current_game(games):
    team_games = ff.add_four_factors(games)
    form = features.team_form(team_games, window=5).set_index(["gameId", "teamId"])

    # Wreck one game's shooting; that game's own form features must not move
    target = team_games.sort_values("date").iloc[30]
    edited = games.copy()
    mask = (edited["gameId"] == target["gameId"]) & (edited["teamId"] == target["teamId"])
    edited.loc[mask, "fieldGoalsMade"] = 0
    edited.loc[mask, "threePointersMade"] = 0
    form_edited = features.team_form(ff.add_four_factors(edited), window=5).set_index(["gameId", "teamId"])

    key = (target["gameId"], target["teamId"])
    pd.testing.assert_series_equal(form.loc[key], form_edited.loc[key])
    later = form.index.get_level_values("teamId") == target["teamId"]
    assert not form.loc[later, "form_efg"].equals(form_edited.loc[later, "form_efg"])


def test_pregame_features_one_row_per_game(games):
    home = data.home_view(games)
    out = features.pregame_features(home, ff.add_four_factors(games), elo.run_elo(home))
    assert len(out) == games["gameId"].nunique()
    assert set(features.FEATURES) <= set(out.columns)


# ---------------------------------------------------------------- evaluation helpers


def test_time_split_is_ordered_and_disjoint():
    df = pd.DataFrame({"season": [2015, 2016, 2017, 2018, 2019], "gameId": range(5)})
    split = models.time_split(df, train_last=2016, validation_last=2017)
    assert split.train["season"].max() < split.validation["season"].min() <= split.validation["season"].max() < split.test["season"].min()
    assert len(split.train) + len(split.validation) + len(split.test) == len(df)


def test_bootstrap_gap_is_zero_for_identical_predictions():
    rng = np.random.default_rng(1)
    y = pd.Series(rng.integers(0, 2, 200))
    p = rng.uniform(0.2, 0.8, 200)
    mean, lo, hi = models.bootstrap_metric_gap(y, p, p, n_boot=50)
    assert mean == lo == hi == 0


def test_calibration_table_of_perfect_forecaster():
    rng = np.random.default_rng(2)
    p = rng.uniform(0, 1, 50_000)
    y = (rng.uniform(0, 1, 50_000) < p).astype(int)
    table = models.calibration_table(pd.Series(y), p)
    assert (table["predicted"] - table["observed"]).abs().max() < 0.03


# ---------------------------------------------------------------- real data (skipped if not downloaded)


@pytest.mark.data
def test_real_data_quality_report(real_data_path):
    games, report = data.prepare_games(real_data_path, 2001, 2024)
    assert report["seasons_dropped"] == {"2021-22": 1}
    assert len(report["games_dropped_inconsistent"]) == 4
    assert report["games"] == games["gameId"].nunique() == 27_597
