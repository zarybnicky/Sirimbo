drop policy if exists my_tenant on skupiny;

comment on table person is '@omit create';

create or replace function skupiny_in_current_tenant(s skupiny) returns boolean language sql stable as $$
  select s.tenant_id = current_tenant_id();
$$;
grant all on function skupiny_in_current_tenant to anonymous;
comment on function skupiny_in_current_tenant is '@filterable';
