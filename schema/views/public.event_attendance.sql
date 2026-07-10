CREATE VIEW public.event_attendance WITH (security_invoker='true') AS
 SELECT id,
    tenant_id,
    instance_id,
    person_id,
    status,
    attendance_note AS note,
    legacy_registration_id AS registration_id,
    event_id,
    attendance_created_at AS created_at,
    attendance_updated_at AS updated_at
   FROM public.event_instance_registration
  WHERE (person_id IS NOT NULL);

COMMENT ON VIEW public.event_attendance IS '
@omit create,update,delete
@behavior -query:resource:list -query:resource:connection -query:resource:single
@simpleCollections only
@primaryKey id
@foreignKey (tenant_id) references tenant (id)
@foreignKey (instance_id) references event_instance (id)|@fieldName instance|@foreignFieldName eventAttendancesByInstanceId
@foreignKey (registration_id) references event_registration (id)|@fieldName registration|@foreignFieldName eventAttendancesByRegistrationId|@behavior -delete
@foreignKey (person_id) references person (id)
';
COMMENT ON COLUMN public.event_attendance.tenant_id IS '@notNull';
COMMENT ON COLUMN public.event_attendance.instance_id IS '@notNull';
COMMENT ON COLUMN public.event_attendance.person_id IS '@notNull';
COMMENT ON COLUMN public.event_attendance.status IS '@notNull';
COMMENT ON COLUMN public.event_attendance.event_id IS '@omit';
COMMENT ON COLUMN public.event_attendance.created_at IS '@notNull';
COMMENT ON COLUMN public.event_attendance.updated_at IS '@notNull';

GRANT SELECT ON TABLE public.event_attendance TO anonymous;
