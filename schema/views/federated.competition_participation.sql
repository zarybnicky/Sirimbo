CREATE VIEW federated.competition_participation WITH (security_invoker='true') AS
 SELECT r.competition_id,
    r.competitor_id,
    COALESCE(e.cancelled, false) AS cancelled,
    r.ranking,
    r.ranking_to,
    r.point_gain,
    r.final_gain,
    r.is_final,
    r.completion_status,
    c.event_id,
    c.start_date AS date,
    r.start_number
   FROM ((federated.competition_result r
     JOIN federated.competition c ON ((c.id = r.competition_id)))
     LEFT JOIN federated.competition_entry e USING (competition_id, competitor_id))
UNION ALL
 SELECT e.competition_id,
    e.competitor_id,
    e.cancelled,
    NULL::integer AS ranking,
    NULL::integer AS ranking_to,
    NULL::numeric(10,3) AS point_gain,
    NULL::numeric(10,3) AS final_gain,
    NULL::boolean AS is_final,
    NULL::text AS completion_status,
    c.event_id,
    c.start_date AS date,
    NULL::text AS start_number
   FROM (federated.competition_entry e
     JOIN federated.competition c ON ((c.id = e.competition_id)))
  WHERE (NOT (EXISTS ( SELECT 1
           FROM federated.competition_result r
          WHERE ((r.competition_id = e.competition_id) AND (r.competitor_id = e.competitor_id)))));

COMMENT ON VIEW federated.competition_participation IS '@name competition_participation
@primaryKey competition_id,competitor_id
@foreignKey (event_id) references federated.event (id)|@fieldName event|@foreignFieldName participations|@notNull
@foreignKey (competition_id) references federated.competition (id)|@fieldName competition|@foreignFieldName participations|@notNull
@foreignKey (competitor_id) references federated.competitor (id)|@fieldName competitor|@foreignFieldName competitionParticipations|@notNull
@behavior -single -list -connection +manyRelation:resource:list -insert -update -delete';
COMMENT ON COLUMN federated.competition_participation.competition_id IS '@notNull';
COMMENT ON COLUMN federated.competition_participation.competitor_id IS '@notNull';
COMMENT ON COLUMN federated.competition_participation.cancelled IS '@notNull';
COMMENT ON COLUMN federated.competition_participation.event_id IS '@notNull';
COMMENT ON COLUMN federated.competition_participation.date IS '@notNull';

GRANT SELECT ON TABLE federated.competition_participation TO anonymous;
