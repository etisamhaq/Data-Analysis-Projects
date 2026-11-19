-- review_id is not unique and some orders have several reviews. Rule: keep each order's most recent review
-- (latest creation date, then latest answer), so every order has at most one score.
select order_id, review_id, review_score, review_created_at
from (
    select
        order_id,
        review_id,
        review_score::int                      as review_score,
        review_creation_date::timestamp        as review_created_at,
        row_number() over (
            partition by order_id
            order by review_creation_date::timestamp desc, review_answer_timestamp::timestamp desc, review_id
        ) as rn
    from raw_reviews
)
where rn = 1
