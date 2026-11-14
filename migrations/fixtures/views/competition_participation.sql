create or replace view federated.competition_participation
with (security_invoker = true) as
select r.competition_id, r.competitor_id, coalesce(e.cancelled, false) as cancelled,
  r.ranking, r.ranking_to, r.point_gain, r.final_gain, r.is_final, r.completion_status,
  c.event_id, c.start_date as date, r.start_number
from federated.competition_result r
join federated.competition c on c.id = r.competition_id
left join federated.competition_entry e using (competition_id, competitor_id)
union all
select e.competition_id, e.competitor_id, e.cancelled,
  null::integer, null::integer, null::numeric(10,3), null::numeric(10,3), null::boolean, null::text,
  c.event_id, c.start_date, null::text
from federated.competition_entry e
join federated.competition c on c.id = e.competition_id
where not exists (
  select 1 from federated.competition_result r
  where (r.competition_id, r.competitor_id) = (e.competition_id, e.competitor_id)
);

comment on view federated.competition_participation is '@name competition_participation
@primaryKey competition_id,competitor_id
@foreignKey (event_id) references federated.event (id)|@fieldName event|@foreignFieldName participations|@notNull
@foreignKey (competition_id) references federated.competition (id)|@fieldName competition|@foreignFieldName participations|@notNull
@foreignKey (competitor_id) references federated.competitor (id)|@fieldName competitor|@foreignFieldName competitionParticipations|@notNull
@behavior -single -list -connection +manyRelation:resource:list -insert -update -delete';
grant select on federated.competition_participation to anonymous;
comment on column federated.competition_participation.competition_id is '@notNull';
comment on column federated.competition_participation.competitor_id is '@notNull';
comment on column federated.competition_participation.cancelled is '@notNull';

comment on column federated.competition_participation.event_id is '@notNull';
comment on column federated.competition_participation.date is '@notNull';
