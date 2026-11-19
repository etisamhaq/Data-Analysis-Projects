-- Payment totals should match item + freight totals; a handful of mismatches exist in the source (vouchers,
-- rounding). Fail only if more than 1% of orders disagree by over 1 BRL.
select share_mismatched
from (
    select avg((abs(payment_value - order_value) > 1)::int) as share_mismatched
    from fct_orders where payment_value is not null and order_value is not null
)
where share_mismatched > 0.01
