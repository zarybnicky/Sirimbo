CREATE or replace FUNCTION person_active_couples(p person) RETURNS SETOF couple as $$
  (select * from couple where man_id = p.id and status = 'active')
  union all
  (select * from couple where woman_id = p.id and status = 'active')
$$ language sql stable;

CREATE or replace FUNCTION person_all_couples(p person) RETURNS SETOF couple as $$
  (select * from couple where man_id = p.id)
  union all
  (select * from couple where woman_id = p.id)
$$ language sql stable;

grant all on function person_active_couples to anonymous;
grant all on function person_all_couples to anonymous;
