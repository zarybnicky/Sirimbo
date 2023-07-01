CREATE VIEW public.akce AS
 SELECT event.id AS a_id,
    event.name AS a_jmeno,
    event.location_text AS a_kde,
    event.description AS a_info,
    event.since AS a_od,
    event.until AS a_do,
    event.capacity AS a_kapacita,
    event.files_legacy AS a_dokumenty,
    event.updated_at AS a_timestamp,
    event.is_locked AS a_lock,
    event.is_visible AS a_visible,
    event.summary,
    event.is_public,
    event.enable_notes
   FROM public.event;

GRANT ALL ON TABLE public.akce TO anonymous;


