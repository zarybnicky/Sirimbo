create or replace view federated.competition_participation
with (security_invoker = true) as
select competition_id, competitor_id from federated.competition_result
union all
select e.competition_id, e.competitor_id
from federated.competition_entry e
where not e.cancelled
  and not exists (
    select 1 from federated.competition_result r
    where (r.competition_id, r.competitor_id) = (e.competition_id, e.competitor_id)
  );

comment on view federated.competition_participation is '@name competition_participation
@primaryKey competition_id,competitor_id
@foreignKey (competition_id) references federated.competition (id)|@fieldName competition|@foreignFieldName participations
@foreignKey (competitor_id) references federated.competitor (id)|@fieldName competitor|@foreignFieldName competitionParticipations
@behavior -single -list -connection -insert -update -delete';
grant select on federated.competition_participation to anonymous;
