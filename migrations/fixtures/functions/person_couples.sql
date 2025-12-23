CREATE or replace FUNCTION person_active_couples(p person) RETURNS SETOF couple as $$
  select *
  from couple
  where (man_id = p.id or woman_id = p.id) and status = 'active'
  order by active_range;
$$ language sql stable;


CREATE or replace FUNCTION person_all_couples(p person) RETURNS SETOF couple as $$
  select *
  from couple
  where (man_id = p.id or woman_id = p.id)
  order by active_range;
$$ language sql stable;

grant all on function person_active_couples to anonymous;
grant all on function person_all_couples to anonymous;
