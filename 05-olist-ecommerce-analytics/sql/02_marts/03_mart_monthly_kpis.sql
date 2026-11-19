-- Complete months only: Jan 2017 to Aug 2018 (2016 is sparse; Sep-Oct 2018 are fragments of an export).
select
    purchase_month                                           as month,
    count(*)                                                 as orders,
    count(distinct customer_unique_id)                       as customers,
    sum(order_value)                                         as gmv,
    avg(order_value)                                         as avg_order_value,
    count(*) filter (where order_number = 1)                 as first_orders,
    count(*) filter (where order_number > 1)                 as repeat_orders,
    avg(late::int) filter (where is_delivered)               as late_rate,
    avg(review_score)                                        as avg_review
from fct_orders
where is_valid and purchase_month between date '2017-01-01' and date '2018-08-01'
group by 1
order by 1
