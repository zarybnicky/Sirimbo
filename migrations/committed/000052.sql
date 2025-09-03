--! Previous: sha1:3c44c5ff02b87ddd5e4330bdd010aa675c8290b8
--! Hash: sha1:d968275009d1e14f194c45ca386b4607be28e1e8

--! split: 1-current.sql
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


CREATE OR REPLACE FUNCTION post_without_cache(input_url TEXT, data JSONB, headers http_header[] = null)
RETURNS http_response LANGUAGE plpgsql AS $$
DECLARE
  new_response http_response;
BEGIN
  SELECT * INTO new_response FROM http(('POST', input_url, headers, 'application/json', data::text));

  RETURN new_response;
END;
$$;
select verify_function('post_without_cache');
comment on function post_without_cache is '@omit';


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

drop function if exists create_person;

CREATE or replace FUNCTION public.create_person(person_id bigint, INOUT p public.person, is_member boolean, is_trainer boolean, is_admin boolean, send_invitation boolean, join_date timestamp with time zone) RETURNS public.person
    LANGUAGE plpgsql
    AS $$
begin
  if person_id is null then
    insert into person (
      first_name,
      last_name,
      gender,
      birth_date,
      nationality,
      tax_identification_number,
      national_id_number,
      csts_id,
      wdsf_id,
      prefix_title,
      suffix_title,
      bio,
      email,
      phone,
      external_ids
    ) values (
      p.first_name,
      p.last_name,
      p.gender,
      p.birth_date,
      p.nationality,
      p.tax_identification_number,
      p.national_id_number,
      p.csts_id,
      p.wdsf_id,
      p.prefix_title,
      p.suffix_title,
      p.bio,
      p.email,
      p.phone,
      p.external_ids
    ) returning * into p;
  else
    select * into p from person where person.id=person_id;
  end if;
  if is_member = true then
    insert into tenant_membership (person_id, tenant_id, since) values (p.id, current_tenant_id(), join_date);
  end if;
  if is_trainer = true then
    insert into tenant_trainer (person_id, tenant_id, since) values (p.id, current_tenant_id(), join_date);
  end if;
  if is_admin = true then
    insert into tenant_administrator (person_id, tenant_id, since) values (p.id, current_tenant_id(), join_date);
  end if;
  if send_invitation = true and p.email is not null and p.email <> '' then
    insert into person_invitation (person_id, tenant_id, email) values (p.id, current_tenant_id(), p.email);
  end if;
end
$$;
select verify_function('create_person');
grant all on function create_person to anonymous;


CREATE or replace FUNCTION app_private.tg_auth_details__refresh() RETURNS TRIGGER security definer AS $$
BEGIN
  perform graphile_worker.add_job('refresh_auth_details', job_key := 'refresh_auth_details');
  return null;
END
$$ LANGUAGE plpgsql;
