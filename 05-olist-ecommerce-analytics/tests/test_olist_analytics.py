"""
The warehouse is tested end to end on a tiny hand-built copy of the source files, with the tricky cases planted:
an order with two reviews, a repeat customer, a cancelled order, an untranslated category, a geocode outside
Brazil and a late delivery. Expected values are worked out by hand. A second set of tests runs on the real data.
"""

from pathlib import Path

import pandas as pd
import pytest
from olist_analytics import analysis, warehouse

REAL_DATA = Path(__file__).resolve().parents[1] / "data"


def write(path: Path, name: str, rows: list[dict]) -> None:
    pd.DataFrame(rows).to_csv(path / name, index=False)


@pytest.fixture
def tiny_data(tmp_path: Path) -> Path:
    orders = [
        # o1: customer A, on time; o2: customer A again (repeat), late; o3: customer B, cancelled
        dict(order_id="o1", customer_id="c1", order_status="delivered", order_purchase_timestamp="2017-03-01 10:00:00",
             order_approved_at="2017-03-01 11:00:00", order_delivered_carrier_date="2017-03-02 10:00:00",
             order_delivered_customer_date="2017-03-08 10:00:00", order_estimated_delivery_date="2017-03-20 00:00:00"),
        dict(order_id="o2", customer_id="c2", order_status="delivered", order_purchase_timestamp="2017-05-10 10:00:00",
             order_approved_at="2017-05-10 11:00:00", order_delivered_carrier_date="2017-05-11 10:00:00",
             order_delivered_customer_date="2017-06-05 10:00:00", order_estimated_delivery_date="2017-06-01 00:00:00"),
        dict(order_id="o3", customer_id="c3", order_status="canceled", order_purchase_timestamp="2017-04-01 10:00:00",
             order_approved_at=None, order_delivered_carrier_date=None, order_delivered_customer_date=None,
             order_estimated_delivery_date="2017-04-20 00:00:00"),
    ]
    customers = [dict(customer_id="c1", customer_unique_id="A", customer_zip_code_prefix=1001, customer_city="sp", customer_state="SP"),
                 dict(customer_id="c2", customer_unique_id="A", customer_zip_code_prefix=1001, customer_city="sp", customer_state="SP"),
                 dict(customer_id="c3", customer_unique_id="B", customer_zip_code_prefix=20000, customer_city="rj", customer_state="RJ")]
    items = [dict(order_id="o1", order_item_id=1, product_id="p1", seller_id="s1", shipping_limit_date="2017-03-03", price=100.0, freight_value=10.0),
             dict(order_id="o1", order_item_id=2, product_id="p2", seller_id="s1", shipping_limit_date="2017-03-03", price=50.0, freight_value=5.0),
             dict(order_id="o2", order_item_id=1, product_id="p2", seller_id="s1", shipping_limit_date="2017-05-12", price=80.0, freight_value=8.0),
             dict(order_id="o3", order_item_id=1, product_id="p1", seller_id="s1", shipping_limit_date="2017-04-03", price=30.0, freight_value=3.0)]
    payments = [dict(order_id="o1", payment_sequential=1, payment_type="credit_card", payment_installments=3, payment_value=165.0),
                dict(order_id="o2", payment_sequential=1, payment_type="boleto", payment_installments=1, payment_value=88.0),
                dict(order_id="o3", payment_sequential=1, payment_type="credit_card", payment_installments=1, payment_value=33.0)]
    reviews = [dict(review_id="r1", order_id="o1", review_score=5, review_comment_title=None, review_comment_message=None,
                    review_creation_date="2017-03-09 00:00:00", review_answer_timestamp="2017-03-10 00:00:00"),
               # o2 has two reviews: the later one (score 1) must win
               dict(review_id="r2", order_id="o2", review_score=4, review_comment_title=None, review_comment_message=None,
                    review_creation_date="2017-06-01 00:00:00", review_answer_timestamp="2017-06-02 00:00:00"),
               dict(review_id="r3", order_id="o2", review_score=1, review_comment_title=None, review_comment_message=None,
                    review_creation_date="2017-06-06 00:00:00", review_answer_timestamp="2017-06-07 00:00:00")]
    products = [dict(product_id="p1", product_category_name="beleza_saude", product_name_lenght=10, product_description_lenght=100,
                     product_photos_qty=1, product_weight_g=500, product_length_cm=10, product_height_cm=10, product_width_cm=10),
                dict(product_id="p2", product_category_name="sem_traducao", product_name_lenght=10, product_description_lenght=100,
                     product_photos_qty=1, product_weight_g=300, product_length_cm=10, product_height_cm=10, product_width_cm=10)]
    sellers = [dict(seller_id="s1", seller_zip_code_prefix=20000, seller_city="rj", seller_state="RJ")]
    geo = [dict(geolocation_zip_code_prefix=1001, geolocation_lat=-23.55, geolocation_lng=-46.63, geolocation_city="sp", geolocation_state="SP"),
           dict(geolocation_zip_code_prefix=1001, geolocation_lat=40.0, geolocation_lng=-3.7, geolocation_city="sp", geolocation_state="SP"),  # bad geocode (Spain)
           dict(geolocation_zip_code_prefix=20000, geolocation_lat=-22.91, geolocation_lng=-43.17, geolocation_city="rj", geolocation_state="RJ")]
    translation = [dict(product_category_name="beleza_saude", product_category_name_english="health_beauty")]
    for name, rows in [("olist_orders_dataset.csv", orders), ("olist_customers_dataset.csv", customers),
                       ("olist_order_items_dataset.csv", items), ("olist_order_payments_dataset.csv", payments),
                       ("olist_order_reviews_dataset.csv", reviews), ("olist_products_dataset.csv", products),
                       ("olist_sellers_dataset.csv", sellers), ("olist_geolocation_dataset.csv", geo),
                       ("product_category_name_translation.csv", translation)]:
        write(tmp_path, name, rows)
    return tmp_path


@pytest.fixture
def tiny_warehouse(tiny_data):
    return warehouse.build(data_dir=tiny_data)


def fct(con) -> pd.DataFrame:
    return warehouse.query(con, "select * from fct_orders order by order_id").set_index("order_id")


def test_all_sql_data_tests_pass_on_clean_data(tiny_warehouse):
    assert all(r.failing_rows == 0 for r in warehouse.run_tests(tiny_warehouse))


def test_order_values_and_main_item(tiny_warehouse):
    f = fct(tiny_warehouse)
    assert f.loc["o1", "items"] == 2
    assert f.loc["o1", "order_value"] == pytest.approx(165.0)
    assert f.loc["o1", "main_category"] == "health_beauty"         # the most expensive item's category
    assert f.loc["o2", "main_category"] == "sem_traducao"           # untranslated: keeps its own name


def test_late_flag_compares_dates(tiny_warehouse):
    f = fct(tiny_warehouse)
    assert not f.loc["o1", "late"] and f.loc["o1", "days_late"] == 0
    assert f.loc["o2", "late"] and f.loc["o2", "days_late"] == 4


def test_latest_review_wins(tiny_warehouse):
    assert fct(tiny_warehouse).loc["o2", "review_score"] == 1


def test_bad_geocodes_are_ignored(tiny_warehouse):
    # Sao Paulo to Rio is about 360 km; including the Spanish point would put it thousands of km away
    assert 300 < fct(tiny_warehouse).loc["o1", "distance_km"] < 420


def test_customers_and_repeat_purchases(tiny_warehouse):
    dim = warehouse.query(tiny_warehouse, "select * from dim_customers").set_index("customer_unique_id")
    assert list(dim.index) == ["A"]                   # B only has a cancelled order
    assert dim.loc["A", "orders"] == 2 and dim.loc["A", "is_repeat"]
    assert not dim.loc["A", "first_order_late"]


def test_cohort_retention(tiny_warehouse):
    cohorts = warehouse.query(tiny_warehouse, "select * from mart_cohort_retention order by months_since")
    assert cohorts["months_since"].tolist() == [0, 2]
    assert cohorts["retention"].tolist() == [1.0, 1.0]


def test_data_test_catches_duplicate_reviews(tiny_data):
    con = warehouse.build(data_dir=tiny_data)
    con.execute("insert into stg_reviews select * from stg_reviews limit 1")
    with pytest.raises(warehouse.DataTestFailure, match="one_review_per_order"):
        warehouse.run_tests(con)


def test_missing_source_file_is_reported(tmp_path):
    with pytest.raises(FileNotFoundError, match="download_data"):
        warehouse.build(data_dir=tmp_path)


def test_model_name_strips_numeric_prefix():
    assert warehouse.model_name(Path("01_fct_orders.sql")) == "fct_orders"
    assert warehouse.model_name(Path("stg_orders.sql")) == "stg_orders"


# ---------------------------------------------------------------- real data (skipped if not downloaded)

@pytest.fixture(scope="module")
def real_warehouse():
    if not (REAL_DATA / "olist_orders_dataset.csv").exists():
        pytest.skip("Olist data not downloaded (run `python download_data.py`)")
    return warehouse.build(data_dir=REAL_DATA)


@pytest.mark.data
def test_real_warehouse_passes_all_data_tests(real_warehouse):
    assert all(r.failing_rows == 0 for r in warehouse.run_tests(real_warehouse))


@pytest.mark.data
def test_real_repeat_rate_is_about_three_percent(real_warehouse):
    rate = warehouse.query(real_warehouse, "select avg(is_repeat::int) as r from dim_customers")["r"][0]
    assert 0.025 < rate < 0.035


@pytest.mark.data
def test_late_delivery_lowers_reviews_after_controls(real_warehouse):
    effect = analysis.late_review_effect(analysis.delivered_reviewed_orders(real_warehouse))
    assert effect.loc["With controls", "ci_high"] < -1.5
