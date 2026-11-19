from pathlib import Path

import numpy as np
import pandas as pd
import pytest
from netflix_analytics import data, recommend, topics, trends

DATA = Path(__file__).resolve().parents[1] / "netflix_titles.csv"


@pytest.fixture(scope="module")
def titles():
    df, report = data.load_titles(DATA)
    return df, report


def toy_titles(n: int = 60) -> pd.DataFrame:
    """Two obvious themes (space vs cooking), half movies and half shows."""
    rows = []
    for i in range(n):
        space = i % 2 == 0
        rows.append({
            "show_id": f"s{i}", "type": "Movie" if i < n // 2 else "TV Show", "title": f"Title {i}",
            "director": "Ann Lee" if space else "Bo Chan", "cast": "X Y, Z W",
            "country": "India, United States" if space else "France", "date_added": "January 1, 2020",
            "release_year": 2019, "rating": "PG" if space else "TV-MA", "duration": "90 min",
            "listed_in": "Sci-Fi Movies" if space else "Cooking Shows",
            "description": ("astronauts fly a rocket to a distant planet galaxy space station orbit" if space
                            else "chefs cook recipes in a kitchen restaurant food competition dinner"),
        })
    return pd.DataFrame(rows)


# ---------------------------------------------------------------- data

@pytest.mark.data
def test_real_file_quality_report(titles):
    df, report = titles
    assert report["titles"] == 8807
    assert len(report["unparsed_dates"]) == 10
    assert set(report["rating_holds_duration"]["rating"]) == {"74 min", "84 min", "66 min"}
    assert df["rating"].astype(str).str.contains("min").sum() == 0     # moved back to duration
    assert str(report["last_date_added"]) == "2021-09-25"
    assert len(report["tv_release_year_after_added"]) == 12      # shows whose latest season postdates arrival
    assert df.loc[df["type"] == "TV Show", "content_age"].isna().all()


def test_validation_rejects_duplicate_ids(tmp_path):
    df = toy_titles()
    df.loc[1, "show_id"] = df.loc[0, "show_id"]
    df.to_csv(tmp_path / "t.csv", index=False)
    with pytest.raises(data.DataValidationError, match="Duplicate"):
        data.load_titles(tmp_path / "t.csv")


def test_every_co_producing_country_is_counted(tmp_path):
    toy_titles().to_csv(tmp_path / "t.csv", index=False)
    df, _ = data.load_titles(tmp_path / "t.csv")
    exploded = data.explode_list(df, "countries", "country")
    assert exploded["country"].value_counts().to_dict() == {"India": 30, "United States": 30, "France": 30}


# ---------------------------------------------------------------- trends

def test_share_by_year_intervals_contain_the_share():
    df = pd.DataFrame({"year_added": [2020] * 100 + [2021] * 100, "is_tv": [1] * 30 + [0] * 70 + [1] * 50 + [0] * 50})
    s = trends.share_by_year(df, "is_tv")
    assert s.loc[2020, "share_%"] == pytest.approx(30)
    assert (s["ci_low_%"] < s["share_%"]).all() and (s["share_%"] < s["ci_high_%"]).all()


def test_trend_test_detects_a_real_trend_and_not_noise():
    rng = np.random.default_rng(0)
    years = np.repeat(np.arange(2016, 2022), 500)
    rising = (rng.uniform(size=len(years)) < 0.2 + 0.05 * (years - 2016)).astype(int)
    flat = (rng.uniform(size=len(years)) < 0.3).astype(int)
    df = pd.DataFrame({"year_added": years, "rising": rising, "flat": flat})
    assert trends.trend_test(df, "rising", 2016, 2021)["p_value"] < 1e-6
    assert trends.trend_test(df, "flat", 2016, 2021)["p_value"] > 0.01


def test_share_change_between_windows():
    items = pd.DataFrame({"year_added": [2016] * 4 + [2020] * 4, "genre": ["a", "a", "a", "b", "a", "b", "b", "b"]})
    change = trends.share_change(items, "genre", (2016, 2016), (2020, 2020))
    assert change["b"] == pytest.approx(50) and change["a"] == pytest.approx(-50)


# ---------------------------------------------------------------- topics

def test_topics_separate_two_obvious_themes():
    df = toy_titles()
    model = topics.TopicModel(*_fit_toy(df))
    space = set(["astronauts", "fly", "rocket", "distant", "planet", "galaxy", "space", "station", "orbit"])
    cooking = set(["chefs", "cook", "recipes", "kitchen", "restaurant", "food", "competition", "dinner"])
    terms = [set(t) for t in model.top_terms(5)]
    # each topic is made of one theme's words only, and both themes appear
    assert all(t <= space or t <= cooking for t in terms)
    assert any(t <= space for t in terms) and any(t <= cooking for t in terms)
    dominant = model.dominant_topic()
    assert len(set(dominant[::2])) == 1 and len(set(dominant[1::2])) == 1 and dominant[0] != dominant[1]


def _fit_toy(df):
    from sklearn.decomposition import NMF
    vectorizer, X = topics.description_matrix(df["description"], min_df=1, max_df=1.0)
    model = NMF(n_components=2, random_state=0, init="nndsvda", max_iter=500)
    return vectorizer, model, model.fit_transform(X)


def test_empty_description_has_no_dominant_topic():
    df = toy_titles()
    df.loc[0, "description"] = ""
    model = topics.TopicModel(*_fit_toy(df))
    assert model.dominant_topic()[0] == -1


# ---------------------------------------------------------------- recommendations

def test_recommender_stays_within_type_and_theme():
    df = toy_titles()
    recs = recommend.ContentRecommender().fit(df).recommend("Title 0", k=5)
    assert set(recs["type"]) == {"Movie"}
    assert set(recs["listed_in"]) == {"Sci-Fi Movies"}
    assert "Title 0" not in set(recs["title"])


def test_recommender_unknown_title():
    with pytest.raises(KeyError):
        recommend.ContentRecommender().fit(toy_titles()).recommend("No Such Title")


@pytest.mark.data
def test_description_recommendations_beat_random_on_real_data(titles):
    df, _ = titles
    result = recommend.genre_agreement_eval(df, n_queries=300, n_boot=200)
    assert result["lift_ci_low"] > 0
