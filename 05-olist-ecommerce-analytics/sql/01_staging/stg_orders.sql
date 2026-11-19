-- One row per order. Timestamps typed; delivery timing derived.
-- `late`: delivered after the date the customer was promised (dates compared, since the estimate has no time).
select
    order_id,
    customer_id,
    order_status                                               as status,
    order_purchase_timestamp::timestamp                        as purchased_at,
    order_approved_at::timestamp                               as approved_at,
    order_delivered_carrier_date::timestamp                    as shipped_at,
    order_delivered_customer_date::timestamp                   as delivered_at,
    order_estimated_delivery_date::date                        as promised_date,
    date_trunc('month', order_purchase_timestamp::timestamp)::date as purchase_month,
    order_status not in ('canceled', 'unavailable')            as is_valid,
    order_status = 'delivered' and order_delivered_customer_date is not null as is_delivered,
    date_diff('day', order_purchase_timestamp::timestamp, order_delivered_customer_date::timestamp) as delivery_days,
    date_diff('day', order_purchase_timestamp::timestamp::date, order_estimated_delivery_date::date) as promised_days,
    case when order_delivered_customer_date is not null
         then order_delivered_customer_date::date > order_estimated_delivery_date::date end as late,
    case when order_delivered_customer_date is not null
         then greatest(date_diff('day', order_estimated_delivery_date::date, order_delivered_customer_date::date), 0) end as days_late
from raw_orders
