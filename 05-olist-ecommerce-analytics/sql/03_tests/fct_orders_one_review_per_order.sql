select order_id, count(*) as n from stg_reviews group by 1 having count(*) > 1
