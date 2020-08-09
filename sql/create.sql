CREATE TABLE IF NOT EXISTS rentals (
    id SERIAL PRIMARY KEY,
    name text,
    type text,
    description text,
    sleeps integer,
    price_per_day bigint,
    home_city text,
    home_state text,
    home_zip text,
    home_county text,
    home_country text,
    vehicle_make text,
    vehicle_model text,
    vehicle_year integer,
    vehicle_length numeric(4,2),
    created timestamp with time zone,
    updated timestamp with time zone,
    lat double precision,
    lng double precision,
    primary_image_url text,
    owner_name text,
    owner_avatar_url text
);



CREATE TABLE IF NOT EXISTS rental_images (
    id SERIAL PRIMARY KEY,
    rental_id integer REFERENCES rentals(id),
    url text
);

CREATE INDEX rental_images_rental_id_fkey ON rental_images(rental_id int4_ops);
