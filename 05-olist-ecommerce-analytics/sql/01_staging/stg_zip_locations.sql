-- The geolocation file has many points per zip prefix and some outside Brazil (bad geocodes).
-- Keep points inside Brazil's bounding box and use each prefix's median position.
select
    lpad(geolocation_zip_code_prefix::varchar, 5, '0') as zip_prefix,
    median(geolocation_lat) as lat,
    median(geolocation_lng) as lng
from raw_geolocation
where geolocation_lat between -34 and 6 and geolocation_lng between -74 and -34
group by 1
