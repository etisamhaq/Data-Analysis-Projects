"""
Builds dashboard/data.json from the warehouse marts, for the static dashboard in dashboard/index.html.

    python dashboard/build_dashboard.py      (from the project folder, after download_data.py)

Every number on the dashboard comes from the same SQL models and analysis functions as the notebook.
"""

import json
from pathlib import Path

from olist_analytics import analysis, warehouse

OUT = Path(__file__).resolve().parent / "data.json"


def records(df):
    return json.loads(df.to_json(orient="records", date_format="iso"))


def main():
    con = warehouse.build()
    q = lambda s: warehouse.query(con, s)  # noqa: E731
    kpis = q("select * from mart_monthly_kpis order by month")
    growth = analysis.yoy_growth(kpis)
    effect = analysis.late_review_effect(analysis.delivered_reviewed_orders(con))
    dose = analysis.lateness_dose_response(analysis.delivered_reviewed_orders(con)).reset_index()
    dose["days_late"] = dose["days_late"].astype(str)

    by_state_month = q("""
        select customer_state as state, strftime(purchase_month, '%Y-%m') as month, count(*) as orders,
               avg(late::int) as late_rate, avg(review_score) as avg_review
        from fct_orders
        where is_delivered and purchase_month between date '2017-01-01' and date '2018-08-01'
        group by 1, 2 order by 1, 2""")
    all_month = q("""
        select 'All states' as state, strftime(purchase_month, '%Y-%m') as month, count(*) as orders,
               avg(late::int) as late_rate, avg(review_score) as avg_review
        from fct_orders
        where is_delivered and purchase_month between date '2017-01-01' and date '2018-08-01'
        group by 2 order by 2""")

    data = {
        "headline": {
            "gmv_2018": growth["gmv_2018"], "gmv_growth": growth["gmv_growth"], "orders_growth": growth["orders_growth"],
            "aov_2018": growth["aov_2018"],
            "repeat_rate": float(q("select avg(is_repeat::int) as r from dim_customers")["r"][0]),
            # Order-weighted: averaging monthly rates would under-weight the busy months when lateness spiked
            "late_rate": float(q("""select avg(late::int) as r from fct_orders where is_delivered
                                    and purchase_month between date '2017-01-01' and date '2018-08-01'""")["r"][0]),
            "late_effect_stars": float(effect.loc["With controls", "effect_stars"]),
            "late_effect_ci": [float(effect.loc["With controls", "ci_low"]), float(effect.loc["With controls", "ci_high"])],
        },
        "monthly": records(kpis.assign(month=kpis["month"].astype(str).str[:7])),
        "delivery_by_state_month": records(all_month) + records(by_state_month),
        "states": records(q("select * from mart_delivery_by_state where orders >= 300 order by late_rate desc")),
        "segments": records(q("""select segment, count(*) as customers, avg(monetary) as avg_spend,
                                 sum(monetary) as total_spend from mart_rfm group by 1 order by total_spend desc""")),
        "dose_response": records(dose),
        "cohorts": records(q("""select strftime(cohort, '%Y-%m') as cohort, months_since, retention
                                from mart_cohort_retention where months_since between 1 and 12""")),
    }
    OUT.write_text(json.dumps(data, separators=(",", ":")))
    print(f"Wrote {OUT} ({OUT.stat().st_size / 1024:.0f} KB)")


if __name__ == "__main__":
    main()
