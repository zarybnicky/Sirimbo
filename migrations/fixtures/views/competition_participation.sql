create or replace view federated.competition_participation
with (security_invoker = true) as
select r.competition_id, r.competitor_id, coalesce(e.cancelled, false) as cancelled
from federated.competition_result r
left join federated.competition_entry e using (competition_id, competitor_id)
union all
select e.competition_id, e.competitor_id, e.cancelled
from federated.competition_entry e
where not exists (
  select 1 from federated.competition_result r
  where (r.competition_id, r.competitor_id) = (e.competition_id, e.competitor_id)
);

comment on view federated.competition_participation is '@name competition_participation
@primaryKey competition_id,competitor_id
@foreignKey (competition_id) references federated.competition (id)|@fieldName competition|@foreignFieldName participations
@foreignKey (competitor_id) references federated.competitor (id)|@fieldName competitor|@foreignFieldName competitionParticipations
@behavior -single -list -connection -insert -update -delete';
grant select on federated.competition_participation to anonymous;
