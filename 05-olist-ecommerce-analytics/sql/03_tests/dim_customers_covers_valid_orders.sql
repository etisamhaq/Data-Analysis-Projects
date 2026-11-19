select f.customer_unique_id
from fct_orders f left join dim_customers d using (customer_unique_id)
where f.is_valid and d.customer_unique_id is null
