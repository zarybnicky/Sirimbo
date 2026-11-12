create or replace view federated.official_assignment
with (security_invoker = true) as
select c.event_id, c.start_date as date, o.person_id, o.role
from federated.competition_official o
join federated.competition c on c.id = o.competition_id
where c.status <> 'cancelled'
union
select o.event_id, e.start_date, o.person_id, o.role
from federated.event_official o
join federated.event e on e.id = o.event_id;

comment on view federated.official_assignment is '@name official_assignment
@primaryKey event_id,date,person_id,role
@foreignKey (event_id) references federated.event (id)|@fieldName event|@foreignFieldName officialAssignments|@notNull
@foreignKey (person_id) references federated.person (id)|@fieldName person|@notNull
@behavior -single -list -connection +manyRelation:resource:list -insert -update -delete';
grant select on federated.official_assignment to anonymous;

comment on column federated.official_assignment.event_id is '@notNull';
comment on column federated.official_assignment.date is '@notNull';
comment on column federated.official_assignment.person_id is '@notNull';
comment on column federated.official_assignment.role is '@notNull';
