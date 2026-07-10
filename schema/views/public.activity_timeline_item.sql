CREATE VIEW public.activity_timeline_item AS
 SELECT NULL::text AS id,
    NULL::public.activity_timeline_kind AS kind,
    NULL::timestamp with time zone AS sort_at,
    NULL::date AS activity_date,
    NULL::bigint AS person_id,
    NULL::text AS person_name,
    NULL::bigint AS event_attendance_id,
    NULL::bigint AS event_instance_id,
    NULL::text AS federation,
    NULL::text AS federated_person_id,
    NULL::text AS competitor_id,
    NULL::text AS competitor_name,
    NULL::federated.competitor_type AS competitor_type,
    NULL::bigint AS competition_event_id,
    NULL::text AS competition_event_name,
    NULL::text AS competition_event_location,
    NULL::bigint AS competition_id,
    NULL::date AS competition_date,
    NULL::time without time zone AS check_in_end,
    NULL::federated.category AS category,
    NULL::text[] AS dances,
    NULL::integer AS participants,
    NULL::integer AS ranking,
    NULL::integer AS ranking_to,
    NULL::numeric(10,3) AS point_gain,
    NULL::boolean AS is_final,
    NULL::federated.competition_type AS competition_type,
    NULL::text AS competition_event_external_id,
    NULL::text AS competition_external_id
  WHERE false;

COMMENT ON VIEW public.activity_timeline_item IS '
@primaryKey id
@interface mode:single type:kind
@type EVENT_ATTENDANCE name:ActivityEventAttendance attributes:event_attendance_id,event_instance_id
@type COMPETITION_BRIEF name:ActivityCompetitionBrief attributes:federation,federated_person_id,competitor_id,competitor_name,competitor_type,competition_event_id,competition_event_name,competition_event_location,competition_id,competition_date,check_in_end,category,dances,participants,competition_type,competition_event_external_id,competition_external_id
@type COMPETITION_RESULT name:ActivityCompetitionResult attributes:federation,federated_person_id,competitor_id,competitor_name,competitor_type,competition_event_id,competition_event_name,competition_event_location,competition_id,competition_date,category,dances,participants,ranking,ranking_to,point_gain,is_final,competition_type,competition_event_external_id,competition_external_id
@type JUDGING name:ActivityJudging attributes:federation,federated_person_id,competition_event_id,competition_event_name,competition_event_location,competition_id,competition_date,category,competition_type,competition_event_external_id,competition_external_id
@type BIRTHDAY name:ActivityBirthday
@foreignKey (person_id) references person (id)|@fieldName person|@behavior -manyRelation:resource:list -manyRelation:resource:connection
@foreignKey (event_attendance_id) references event_attendance (id)|@fieldName eventAttendance|@behavior -manyRelation:resource:list -manyRelation:resource:connection
@foreignKey (event_attendance_id) references event_instance_registration (id)|@fieldName eventInstanceRegistration|@behavior -manyRelation:resource:list -manyRelation:resource:connection
@foreignKey (event_instance_id) references event_instance (id)|@fieldName eventInstance|@behavior -manyRelation:resource:list -manyRelation:resource:connection
@behavior -query:resource:list -query:resource:connection -query:resource:single
';

GRANT SELECT ON TABLE public.activity_timeline_item TO anonymous;
