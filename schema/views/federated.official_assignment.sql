CREATE VIEW federated.official_assignment WITH (security_invoker='true') AS
 SELECT c.event_id,
    c.start_date AS date,
    o.person_id,
    o.role
   FROM (federated.competition_official o
     JOIN federated.competition c ON ((c.id = o.competition_id)))
  WHERE (c.status <> 'cancelled'::federated.event_status)
UNION
 SELECT o.event_id,
    e.start_date AS date,
    o.person_id,
    o.role
   FROM (federated.event_official o
     JOIN federated.event e ON ((e.id = o.event_id)));

COMMENT ON VIEW federated.official_assignment IS '@name official_assignment
@primaryKey event_id,date,person_id,role
@foreignKey (event_id) references federated.event (id)|@fieldName event|@foreignFieldName officialAssignments|@notNull
@foreignKey (person_id) references federated.person (id)|@fieldName person|@notNull
@behavior -single -list -connection +manyRelation:resource:list -insert -update -delete';
COMMENT ON COLUMN federated.official_assignment.event_id IS '@notNull';
COMMENT ON COLUMN federated.official_assignment.date IS '@notNull';
COMMENT ON COLUMN federated.official_assignment.person_id IS '@notNull';
COMMENT ON COLUMN federated.official_assignment.role IS '@notNull';

GRANT SELECT ON TABLE federated.official_assignment TO anonymous;
