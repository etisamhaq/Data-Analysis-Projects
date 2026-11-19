-- customer_id is per order; customer_unique_id identifies the person, so repeat purchases use it.
select
    customer_id,
    customer_unique_id,
    lpad(customer_zip_code_prefix::varchar, 5, '0') as zip_prefix,
    customer_city                                    as city,
    customer_state                                   as state
from raw_customers
