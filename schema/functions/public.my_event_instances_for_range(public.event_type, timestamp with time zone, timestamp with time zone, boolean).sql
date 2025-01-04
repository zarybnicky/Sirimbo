CREATE FUNCTION public.my_event_instances_for_range(only_type public.event_type, start_range timestamp with time zone, end_range timestamp with time zone DEFAULT NULL::timestamp with time zone, only_mine boolean DEFAULT false) RETURNS SETOF public.event_instance
    LANGUAGE sql STABLE
    BEGIN ATOMIC
 SELECT DISTINCT ON (instances.id) instances.id,
     instances.tenant_id,
     instances.event_id,
     instances.created_at,
     instances.updated_at,
     instances.since,
     instances.until,
     instances.range,
     instances.location_id,
     instances.is_cancelled
    FROM (((public.event_instances_for_range(my_event_instances_for_range.only_type, my_event_instances_for_range.start_range, my_event_instances_for_range.end_range) instances(id, tenant_id, event_id, created_at, updated_at, since, until, range, location_id, is_cancelled)
      LEFT JOIN public.event_registration ON (((event_registration.event_id = instances.event_id) AND ((event_registration.person_id = ANY (public.current_person_ids())) OR (event_registration.couple_id = ANY (public.current_couple_ids()))))))
      LEFT JOIN public.event_trainer ON (((event_trainer.event_id = instances.event_id) AND (event_trainer.person_id = ANY (public.current_person_ids())))))
      LEFT JOIN public.event_instance_trainer ON (((event_instance_trainer.instance_id = instances.id) AND (event_instance_trainer.person_id = ANY (public.current_person_ids())))))
   WHERE ((event_registration.id IS NOT NULL) OR (event_trainer.id IS NOT NULL) OR (event_instance_trainer.id IS NOT NULL));
END;

COMMENT ON FUNCTION public.my_event_instances_for_range(only_type public.event_type, start_range timestamp with time zone, end_range timestamp with time zone, only_mine boolean) IS '@simpleCollections only';

GRANT ALL ON FUNCTION public.my_event_instances_for_range(only_type public.event_type, start_range timestamp with time zone, end_range timestamp with time zone, only_mine boolean) TO anonymous;
