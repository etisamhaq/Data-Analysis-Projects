-- Recency, frequency, monetary segmentation as of the day after the last order in the data.
-- With 97% one-time buyers, frequency barely varies, so it enters as "1 order" vs "2+".
with base as (
    select
        customer_unique_id,
        date_diff('day', last_order_at, (select max(purchased_at) from fct_orders) + interval 1 day) as recency_days,
        orders                                                      as frequency,
        total_value                                                 as monetary
    from dim_customers
),
scored as (
    select *,
        ntile(5) over (order by recency_days desc) as r_score,   -- 5 = most recent
        ntile(5) over (order by monetary)          as m_score    -- 5 = highest spend
    from base
)
select *,
    case
        when frequency > 1 and r_score >= 4                  then 'Loyal, active'
        when frequency > 1                                   then 'Loyal, lapsing'
        when r_score >= 4 and m_score >= 4                   then 'New, high value'
        when r_score >= 4                                    then 'New, low value'
        when m_score >= 4                                    then 'Lapsed, high value'
        else 'Lapsed, low value'
    end as segment
from scored
