-- Brazil is about 4,400 km across; longer customer-seller distances mean a bad geocode slipped through
select order_id, distance_km from fct_orders where distance_km > 4500
