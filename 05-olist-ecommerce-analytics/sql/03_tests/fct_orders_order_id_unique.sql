select order_id, count(*) as n from fct_orders group by 1 having count(*) > 1
