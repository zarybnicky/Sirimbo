create or replace function tenant_couples(t tenant) returns setof couple as $$
  select couple.*
  from couple
  where couple.status = 'active'
    and (exists (select 1 from current_tenant_membership where person_id = man_id)
      or exists (select 1 from current_tenant_membership where person_id = woman_id));
$$ language sql stable;
grant all on function tenant_couples to anonymous;
comment on function tenant_couples is '@simpleCollections only';
