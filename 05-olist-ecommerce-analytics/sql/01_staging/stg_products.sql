-- English category names; untranslated categories keep their Portuguese name, missing ones become 'unknown'.
select
    p.product_id,
    coalesce(t.product_category_name_english, p.product_category_name, 'unknown') as category,
    p.product_weight_g::double as weight_g
from raw_products p
left join raw_category_translation t using (product_category_name)
