-- One row per order with value, delivery, review and geography. The "main" item is the most expensive one;
-- its category and seller describe the order. Distance is customer-to-main-seller, great-circle, in km.
with items as (
    select
        order_id,
        count(*)                     as items,
        count(distinct seller_id)    as sellers,
        sum(price)                   as item_value,
        sum(freight)                 as freight_value
    from stg_order_items
    group by order_id
),
main_item as (
    select order_id, product_id, seller_id
    from (select *, row_number() over (partition by order_id order by price desc, order_item_id) as rn
          from stg_order_items)
    where rn = 1
),
payments as (
    select order_id, sum(payment_value) as payment_value, max(payment_installments) as installments,
           arg_max(payment_type, payment_value) as main_payment_type
    from stg_payments
    group by order_id
),
located as (
    select
        m.order_id,
        2 * 6371 * asin(sqrt(
            pow(sin(radians(sl.lat - cl.lat) / 2), 2)
            + cos(radians(cl.lat)) * cos(radians(sl.lat)) * pow(sin(radians(sl.lng - cl.lng) / 2), 2)
        )) as distance_km
    from main_item m
    join stg_orders o using (order_id)
    join stg_customers c using (customer_id)
    join stg_sellers s on s.seller_id = m.seller_id
    join stg_zip_locations cl on cl.zip_prefix = c.zip_prefix
    join stg_zip_locations sl on sl.zip_prefix = s.zip_prefix
)
select
    o.order_id,
    c.customer_unique_id,
    c.state                                    as customer_state,
    o.status,
    o.is_valid,
    o.is_delivered,
    o.purchased_at,
    o.purchase_month,
    i.items,
    i.sellers,
    i.item_value,
    i.freight_value,
    i.item_value + i.freight_value             as order_value,
    p.payment_value,
    p.installments,
    p.main_payment_type,
    pr.category                                as main_category,
    s.state                                    as seller_state,
    l.distance_km,
    o.delivery_days,
    o.promised_days,
    o.late,
    o.days_late,
    r.review_score,
    row_number() over (partition by c.customer_unique_id, o.is_valid order by o.purchased_at, o.order_id) as order_number
from stg_orders o
join stg_customers c using (customer_id)
left join items i using (order_id)
left join main_item m using (order_id)
left join stg_products pr on pr.product_id = m.product_id
left join stg_sellers s on s.seller_id = m.seller_id
left join payments p using (order_id)
left join located l using (order_id)
left join stg_reviews r using (order_id)
