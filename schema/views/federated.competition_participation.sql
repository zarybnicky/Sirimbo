CREATE VIEW federated.competition_participation WITH (security_invoker='true') AS
 SELECT r.competition_id,
    r.competitor_id,
    COALESCE(e.cancelled, false) AS cancelled
   FROM (federated.competition_result r
     LEFT JOIN federated.competition_entry e USING (competition_id, competitor_id))
UNION ALL
 SELECT e.competition_id,
    e.competitor_id,
    e.cancelled
   FROM federated.competition_entry e
  WHERE (NOT (EXISTS ( SELECT 1
           FROM federated.competition_result r
          WHERE ((r.competition_id = e.competition_id) AND (r.competitor_id = e.competitor_id)))));

COMMENT ON VIEW federated.competition_participation IS '@name competition_participation
@primaryKey competition_id,competitor_id
@foreignKey (competition_id) references federated.competition (id)|@fieldName competition|@foreignFieldName participations
@foreignKey (competitor_id) references federated.competitor (id)|@fieldName competitor|@foreignFieldName competitionParticipations
@behavior -single -list -connection -insert -update -delete';

GRANT SELECT ON TABLE federated.competition_participation TO anonymous;
