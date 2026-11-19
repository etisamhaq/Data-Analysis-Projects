select
    seller_id,
    lpad(seller_zip_code_prefix::varchar, 5, '0') as zip_prefix,
    seller_city  as city,
    seller_state as state
from raw_sellers
