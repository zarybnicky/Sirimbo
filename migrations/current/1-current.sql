
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

-- Rename event table to event_series for future-proof naming
DO $$
BEGIN
  IF to_regclass('public.event_series') IS NULL AND to_regclass('public.event') IS NOT NULL THEN
    EXECUTE 'alter table public.event rename to event_series';
  END IF;
END;
$$;

-- Keep identity sequence aligned with the new table name when the old sequence exists
DO $$
BEGIN
  IF to_regclass('public.event_id_seq') IS NOT NULL AND to_regclass('public.event_series_id_seq') IS NULL THEN
    EXECUTE 'alter sequence public.event_id_seq rename to event_series_id_seq';
  END IF;
END;
$$;

-- Recreate the event compatibility view so legacy code keeps working
CREATE OR REPLACE VIEW public.event AS
SELECT *
FROM public.event_series;

COMMENT ON VIEW public.event IS E'@omit create\n@primaryKey id\n@foreignKey (tenant_id) references tenant (id)\n@foreignKey (location_id) references tenant_location (id)\n@foreignKey (payment_recipient_id) references account (id)';

-- Rename event_registration to event_series_registration and align its columns
DO $$
BEGIN
  IF to_regclass('public.event_series_registration') IS NULL AND to_regclass('public.event_registration') IS NOT NULL THEN
    EXECUTE 'alter table public.event_registration rename to event_series_registration';
  END IF;
END;
$$;

DO $$
BEGIN
  IF to_regclass('public.event_registration_id_seq') IS NOT NULL AND to_regclass('public.event_series_registration_id_seq') IS NULL THEN
    EXECUTE 'alter sequence public.event_registration_id_seq rename to event_series_registration_id_seq';
  END IF;
END;
$$;

DO $$
BEGIN
  IF EXISTS (
    SELECT 1
    FROM information_schema.columns
    WHERE table_schema = 'public' AND table_name = 'event_series_registration' AND column_name = 'event_id'
  ) AND NOT EXISTS (
    SELECT 1
    FROM information_schema.columns
    WHERE table_schema = 'public' AND table_name = 'event_series_registration' AND column_name = 'event_series_id'
  ) THEN
    EXECUTE 'alter table public.event_series_registration rename column event_id to event_series_id';
  END IF;
END;
$$;

-- Compatibility view exposing the legacy event_registration shape for PostGraphile
CREATE OR REPLACE VIEW public.event_registration AS
SELECT
  id,
  tenant_id,
  event_series_id AS event_id,
  target_cohort_id,
  couple_id,
  person_id,
  note,
  created_at,
  updated_at
FROM public.event_series_registration;

COMMENT ON VIEW public.event_registration IS E'@omit update\n@simpleCollections both\n@primaryKey id\n@foreignKey (tenant_id) references tenant (id)\n@foreignKey (event_id) references event (id)\n@foreignKey (target_cohort_id) references event_target_cohort (id)\n@foreignKey (couple_id) references couple (id)\n@foreignKey (person_id) references person (id)';
