-- Delivered before it was bought would mean broken timestamps
select order_id, delivery_days from fct_orders where delivery_days < 0
