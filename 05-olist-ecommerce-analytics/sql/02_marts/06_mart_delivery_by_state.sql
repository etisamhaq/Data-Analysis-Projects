-- Delivery performance by customer state (delivered orders, analysis window).
select
    customer_state                               as state,
    count(*)                                     as orders,
    avg(late::int)                               as late_rate,
    median(delivery_days)                        as median_delivery_days,
    median(promised_days)                        as median_promised_days,
    median(distance_km)                          as median_distance_km,
    avg(review_score)                            as avg_review
from fct_orders
where is_delivered and purchase_month between date '2017-01-01' and date '2018-08-01'
group by 1
order by orders desc
