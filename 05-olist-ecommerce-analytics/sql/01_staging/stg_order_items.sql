select
    order_id,
    order_item_id,
    product_id,
    seller_id,
    price::double         as price,
    freight_value::double as freight
from raw_order_items
