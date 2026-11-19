select order_id, item_value, freight_value from fct_orders where item_value < 0 or freight_value < 0
