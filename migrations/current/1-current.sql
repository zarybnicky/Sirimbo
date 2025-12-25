drop index if exists cohort_membership_range_idx;
drop index if exists couple_range_idx;
drop index if exists user_proxy_range_idx;
drop index if exists tenant_administrator_range_idx;
drop index if exists tenant_membership_range_idx;
drop index if exists tenant_trainer_range_idx;

drop index if exists idx_cm_tenant;
drop index if exists cohort_membership_cohort_id_idx;
drop index if exists cohort_membership_person_id_idx;

create index if not exists tenant_administrator_person_id_idx on tenant_administrator (person_id);
create index if not exists tenant_administrator_tenant_id_idx on tenant_administrator (tenant_id);

drop index if exists cohort_membership_status_idx;
drop index if exists tenant_membership_status_idx;

drop index if exists couple_man_active_idx;
drop index if exists couple_woman_active_idx;
create index if not exists couple_man_active_idx on couple (man_id) include (id) where status = 'active';
create index if not exists couple_woman_active_idx on couple (woman_id) include (id) where status = 'active';
create index if not exists couple_woman_active_lookup on public.couple (woman_id, since, until, id) include (man_id, status) where status='active';
create index if not exists couple_man_active_lookup on public.couple (man_id, since, until, id) include (woman_id, status) where status='active';

create index if not exists cohort_membership_person_id_id_idx on public.cohort_membership (person_id, id) include (cohort_id);
create index if not exists cohort_membership_active_by_person on cohort_membership (person_id) include (cohort_id) where status = 'active';
create index if NOT EXISTS cohort_membership_tenant_status_person_idx on cohort_membership (tenant_id, status, person_id, cohort_id);

create index if not exists tenant_membership_active_by_person on tenant_membership (person_id) include (tenant_id) where status = 'active';
create index if not exists tenant_membership_active_by_tenant on tenant_membership (tenant_id) include (person_id) where status = 'active';
create index if not exists tenant_membership_tenant_person_active_idx on tenant_membership (tenant_id, person_id) where status = 'active';
create index if NOT EXISTS tenant_membership_tenant_status_person_idx on tenant_membership (tenant_id, status, person_id);

create index if not exists tenant_trainer_active_by_person on tenant_trainer (person_id) include (tenant_id) where status = 'active';
create index if not exists tenant_trainer_active_by_tenant on tenant_trainer (tenant_id) include (person_id) where status = 'active';
create index if not exists tenant_trainer_tenant_person_active_idx on tenant_trainer (tenant_id, person_id) where status = 'active';
create index if NOT EXISTS tenant_trainer_tenant_status_person_idx on tenant_trainer (tenant_id, status, person_id);

create index if not exists tenant_administrator_active_by_person on tenant_administrator (person_id) include (tenant_id) where status = 'active';
create index if not exists tenant_administrator_active_by_tenant on tenant_administrator (tenant_id) include (person_id) where status = 'active';
create index if not exists tenant_administrator_tenant_person_active_idx on tenant_administrator (tenant_id, person_id) where status = 'active';
create index if NOT EXISTS tenant_administrator_tenant_status_person_idx on tenant_administrator (tenant_id, status, person_id);

select app_private.drop_policies('public.person');
select app_private.drop_policies('public.couple');
drop function if exists app_private.visible_person_ids();

--!include functions/visible_person_ids.sql
--!include functions/current_person_ids.sql
--!include policies/couple.sql
--!include policies/person.sql
--!include functions/filtered_people.sql
--!include functions/create_jwt_token.sql
--!include functions/queue_announcement_notification.sql

drop MATERIALIZED VIEW IF EXISTS public.auth_details;
drop view IF EXISTS auth_details_view;
drop function if exists former_filtered_people(is_trainer boolean, is_admin boolean, in_cohorts bigint[]);

alter table if exists public.galerie_foto SET SCHEMA app_private;

CREATE or replace FUNCTION person_is_admin(p person) RETURNS boolean AS $$
  select exists (select 1 from tenant_administrator where person_id = p.id and tenant_id = current_tenant_id() and status = 'active');
$$ language sql stable PARALLEL SAFE LEAKPROOF;
GRANT ALL ON FUNCTION person_is_admin TO anonymous;

CREATE or replace FUNCTION person_is_trainer(p person) RETURNS boolean AS $$
  select exists (select 1 from tenant_trainer where person_id = p.id and tenant_id = current_tenant_id() and status = 'active');
$$ language sql stable PARALLEL SAFE LEAKPROOF;
GRANT ALL ON FUNCTION person_is_admin TO anonymous;

CREATE or replace FUNCTION person_is_member(p person) RETURNS boolean AS $$
  select exists (select 1 from tenant_membership where person_id = p.id and tenant_id = current_tenant_id() and status = 'active');
$$ language sql stable PARALLEL SAFE LEAKPROOF;
GRANT ALL ON FUNCTION person_is_member TO anonymous;
