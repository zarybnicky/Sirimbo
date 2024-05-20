CREATE FUNCTION public.my_event_instances_for_range(only_type public.event_type, start_range timestamp with time zone, end_range timestamp with time zone DEFAULT NULL::timestamp with time zone, only_mine boolean DEFAULT false) RETURNS SETOF public.event_instance
    LANGUAGE sql STABLE
    BEGIN ATOMIC
 SELECT DISTINCT ON (event_instance.id) event_instance.id,
     event_instance.tenant_id,
     event_instance.event_id,
     event_instance.created_at,
     event_instance.updated_at,
     event_instance.since,
     event_instance.until,
     event_instance.range,
     event_instance.location_id,
     event_instance.is_cancelled
    FROM ((((public.event_instance
      JOIN public.event ON ((event_instance.event_id = event.id)))
      LEFT JOIN public.event_registration ON ((event_registration.event_id = event.id)))
      LEFT JOIN public.event_trainer ON ((event_trainer.event_id = event.id)))
      LEFT JOIN public.event_instance_trainer ON ((event_instance_trainer.instance_id = event_instance.id)))
   WHERE (event.is_visible AND (tstzrange(my_event_instances_for_range.start_range, my_event_instances_for_range.end_range, '[]'::text) && event_instance.range) AND ((my_event_instances_for_range.only_type IS NULL) OR (event.type = my_event_instances_for_range.only_type)) AND ((event_registration.person_id IN ( SELECT public.my_person_ids() AS my_person_ids)) OR (event_registration.couple_id IN ( SELECT public.my_couple_ids() AS my_couple_ids)) OR (event_trainer.person_id IN ( SELECT public.my_person_ids() AS my_person_ids)) OR (event_instance_trainer.person_id IN ( SELECT public.my_person_ids() AS my_person_ids))));
END;

COMMENT ON FUNCTION public.my_event_instances_for_range(only_type public.event_type, start_range timestamp with time zone, end_range timestamp with time zone, only_mine boolean) IS '@simpleCollections only';

GRANT ALL ON FUNCTION public.my_event_instances_for_range(only_type public.event_type, start_range timestamp with time zone, end_range timestamp with time zone, only_mine boolean) TO anonymous;


