-- One row per customer (customer_unique_id), from valid orders only.
select
    customer_unique_id,
    min(purchased_at)                                        as first_order_at,
    max(purchased_at)                                        as last_order_at,
    count(*)                                                 as orders,
    sum(order_value)                                         as total_value,
    count(*) > 1                                             as is_repeat,
    arg_min(customer_state, purchased_at)                    as state,
    arg_min(main_category, purchased_at)                     as first_category,
    arg_min(order_value, purchased_at)                       as first_order_value,
    arg_min(late, purchased_at)                              as first_order_late,
    arg_min(review_score, purchased_at)                      as first_order_review,
    arg_min(delivery_days, purchased_at)                     as first_order_delivery_days
from fct_orders
where is_valid
group by customer_unique_id
