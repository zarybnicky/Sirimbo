create table if not exists public.tenant_settings (
    tenant_id bigint DEFAULT public.current_tenant_id() NOT NULL REFERENCES tenant(id) ON DELETE CASCADE primary key,
    settings jsonb not null
);

COMMENT ON TABLE tenant_settings is E'@simpleCollections only\n@omit create,delete';
GRANT ALL ON TABLE tenant_settings TO anonymous;
ALTER TABLE tenant_settings ENABLE ROW LEVEL SECURITY;

select app_private.drop_policies('public.tenant_settings');
CREATE POLICY current_tenant ON tenant_settings AS RESTRICTIVE USING ((tenant_id = (SELECT public.current_tenant_id())));
CREATE POLICY admin_own ON tenant_settings TO administrator USING (true) WITH CHECK (true);

insert into tenant_settings
select id, '{}'::jsonb from tenant
on conflict do nothing;


-- triggers - on create tenant, create tenant_settings


--!include functions/post_without_cache.sql

create or replace function update_tenant_settings_key(path text[], new_value jsonb) returns tenant_settings language sql as $$
  update tenant_settings
  set settings = jsonb_set(settings, path, new_value, true)
  where tenant_id=current_tenant_id()
  returning *;
$$;
revoke all on function update_tenant_settings_key from anonymous;
grant all on function update_tenant_settings_key to administrator;

create or replace function archive_cohort(bigint) returns cohort language sql as $$
  update cohort_membership set until=now() where cohort_id = $1;
  update cohort
  set is_visible = false, cohort_group_id = null
  where id = $1
  returning *;
$$;

alter table cohort
  add column if not exists external_ids text[] null default null;
alter table person
  add column if not exists external_ids text[] null default null;
