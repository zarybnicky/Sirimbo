
--!include functions/event_instance_approx_price.sql

CREATE OR REPLACE FUNCTION on_update_author_aktuality() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
BEGIN
   NEW.at_kdo = current_user_id();
   RETURN NEW;
END;
$$;

BEGIN;
ALTER TABLE aktuality DISABLE TRIGGER ALL;

UPDATE aktuality SET at_foto = null WHERE at_foto = 0;
UPDATE aktuality SET at_foto = null WHERE at_foto_main = 0 OR at_foto_main IS null;

update aktuality set title_photo_url = '/galerie/' || (select gf_path from galerie_foto where id = at_foto_main)
where title_photo_url is null and at_foto_main is not null;

ALTER TABLE aktuality ENABLE TRIGGER ALL;
COMMIT;
