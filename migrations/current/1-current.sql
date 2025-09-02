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

create or replace function archive_cohort(cohort_id bigint) returns cohort language sql as $$
  update cohort_membership set until=now() where cohort_id = $1;
  update cohort
  set is_visible = false, cohort_group_id = null
  where id = $1
  returning *;
$$;
revoke all on function archive_cohort from anonymous;
grant all on function archive_cohort to administrator;

create or replace function sync_cohort_memberships(person_id bigint, cohort_ids bigint[]) returns void language sql as $$
  update cohort_membership set until = now(), status = 'expired'
  where active and person_id = $1 and cohort_id <> all (cohort_ids);

  insert into cohort_membership (status, since, person_id, cohort_id)
  select 'active', NOW(), $1, new_cohort_id
  from unnest(cohort_ids) as x(new_cohort_id)
  where not exists (select 1 from cohort_membership where active and person_id = $1 and cohort_id = new_cohort_id);
$$;
revoke all on function sync_cohort_memberships from anonymous;
grant all on function sync_cohort_memberships to administrator;

alter table cohort
  add column if not exists external_ids text[] null default null;
alter table person
  add column if not exists external_ids text[] null default null;
