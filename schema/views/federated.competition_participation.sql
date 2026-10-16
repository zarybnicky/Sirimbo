CREATE VIEW federated.competition_participation WITH (security_invoker='true') AS
 SELECT competition_result.competition_id,
    competition_result.competitor_id
   FROM federated.competition_result
UNION ALL
 SELECT e.competition_id,
    e.competitor_id
   FROM federated.competition_entry e
  WHERE ((NOT e.cancelled) AND (NOT (EXISTS ( SELECT 1
           FROM federated.competition_result r
          WHERE ((r.competition_id = e.competition_id) AND (r.competitor_id = e.competitor_id))))));

COMMENT ON VIEW federated.competition_participation IS '@primaryKey competition_id,competitor_id
@foreignKey (competition_id) references federated.competition (id)|@fieldName competition|@foreignFieldName participations
@foreignKey (competitor_id) references federated.competitor (id)|@fieldName competitor|@foreignFieldName competitionParticipations
@behavior -single -list -connection -insert -update -delete';

GRANT SELECT ON TABLE federated.competition_participation TO anonymous;
