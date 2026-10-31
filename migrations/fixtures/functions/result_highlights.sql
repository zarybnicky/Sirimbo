create or replace function app_private.result_highlights(competitor_ids text[])
returns setof federated.competition_result
language sql stable as $$
  with ranked as (
    select r as result, c.federation, c.competition_type, c.start_date, c.id,
      rank() over (
        partition by c.federation, c.category_id, c.competition_type
        order by coalesce(r.ranking_to, r.ranking), r.ranking
      ) as position
    from federated.competition_result r
    join federated.competition c on c.id = r.competition_id
    join federated.event e on e.id = c.event_id
    where r.competitor_id = any(competitor_ids)
      and c.status = 'completed' and e.status <> 'cancelled'
      and r.ranking > 0
      and coalesce(r.completion_status, '') not in ('Disqualified', 'Excused', 'Noshow', 'Withdrawn')
  )
  select (result).* from ranked
  where position = 1
  order by (federation = 'wdsf') desc,
    (competition_type = 'championship') desc nulls last,
    coalesce((result).ranking_to, (result).ranking), (result).ranking, start_date desc, id desc;
$$;

grant execute on function app_private.result_highlights to anonymous;

create or replace function federated.competitor_result_highlights(competitor federated.competitor)
returns setof federated.competition_result
language sql stable as $$
  select * from app_private.result_highlights(array[competitor.id]);
$$;

comment on function federated.competitor_result_highlights is '@behavior +list -connection
@notNull';
grant execute on function federated.competitor_result_highlights to anonymous;

create or replace function federated.person_result_highlights(person federated.person)
returns setof federated.competition_result
language sql stable as $$
  select * from app_private.result_highlights(array(
    select competitor_id from federated.competitor_component where person_id = person.id
  ));
$$;

comment on function federated.person_result_highlights is '@behavior +list -connection
@notNull';
grant execute on function federated.person_result_highlights to anonymous;
