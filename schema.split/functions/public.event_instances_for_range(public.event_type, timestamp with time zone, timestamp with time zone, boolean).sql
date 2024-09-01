CREATE FUNCTION public.event_instances_for_range(only_type public.event_type, start_range timestamp with time zone, end_range timestamp with time zone DEFAULT NULL::timestamp with time zone, only_mine boolean DEFAULT false) RETURNS SETOF public.event_instance
    LANGUAGE sql STABLE
    BEGIN ATOMIC
 SELECT event_instance.id,
     event_instance.tenant_id,
     event_instance.event_id,
     event_instance.created_at,
     event_instance.updated_at,
     event_instance.since,
     event_instance.until,
     event_instance.range,
     event_instance.location_id,
     event_instance.is_cancelled
    FROM (public.event_instance
      JOIN public.event ON ((event_instance.event_id = event.id)))
   WHERE (event.is_visible AND (event_instance.since <= event_instances_for_range.end_range) AND (event_instance.until >= event_instances_for_range.start_range) AND ((event_instances_for_range.only_type IS NULL) OR (event.type = event_instances_for_range.only_type)));
END;

COMMENT ON FUNCTION public.event_instances_for_range(only_type public.event_type, start_range timestamp with time zone, end_range timestamp with time zone, only_mine boolean) IS '@simpleCollections only';

GRANT ALL ON FUNCTION public.event_instances_for_range(only_type public.event_type, start_range timestamp with time zone, end_range timestamp with time zone, only_mine boolean) TO anonymous;


