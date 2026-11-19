-- Monthly acquisition cohorts: share of each cohort ordering again N months after their first order.
with first_orders as (
    select customer_unique_id, date_trunc('month', first_order_at)::date as cohort
    from dim_customers
),
activity as (
    select distinct f.customer_unique_id, fo.cohort,
           date_diff('month', fo.cohort, f.purchase_month) as months_since
    from fct_orders f
    join first_orders fo using (customer_unique_id)
    where f.is_valid
)
select
    cohort,
    months_since,
    count(*)                                                        as customers,
    count(*) / max(count(*)) over (partition by cohort)             as retention
from activity
where cohort between date '2017-01-01' and date '2018-08-01'
group by 1, 2
order by 1, 2
