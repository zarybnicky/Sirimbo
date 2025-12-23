create or replace function tenant_couples(t tenant) returns setof couple as $$
  select distinct couple.*
  from couple
  join tenant_membership on man_id = person_id or woman_id = person_id
  where couple.status = 'active' and tenant_membership.status = 'active' and tenant_id = t.id
  order by couple.active_range asc;
$$ language sql stable;
grant all on function tenant_couples to anonymous;
comment on function tenant_couples is '@simpleCollections only';
