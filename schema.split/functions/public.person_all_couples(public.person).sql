CREATE FUNCTION public.person_all_couples(p public.person) RETURNS SETOF public.couple
    LANGUAGE sql STABLE
    BEGIN ATOMIC
 SELECT couple.id,
     couple.man_id,
     couple.woman_id,
     couple.since,
     couple.until,
     couple.created_at,
     couple.updated_at,
     couple.legacy_pary_id,
     couple.active_range,
     couple.status,
     couple.active
    FROM public.couple
   WHERE ((couple.man_id = (person_all_couples.p).id) OR (couple.woman_id = (person_all_couples.p).id))
   ORDER BY couple.active_range;
END;

COMMENT ON FUNCTION public.person_all_couples(p public.person) IS '@simpleCollections only';

GRANT ALL ON FUNCTION public.person_all_couples(p public.person) TO anonymous;
