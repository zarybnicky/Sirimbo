drop index if exists cohort_membership_range_idx;
drop index if exists couple_range_idx;
drop index if exists user_proxy_range_idx;
drop index if exists tenant_administrator_range_idx;
drop index if exists tenant_membership_range_idx;
drop index if exists tenant_trainer_range_idx;

drop index if exists cohort_membership.idx_cm_tenant;
drop index if exists cohort_membership.cohort_membership_cohort_id_idx;
drop index if exists cohort_membership.cohort_membership_person_id_idx;

create index if not exists cohort_membership_person_id_id_idx on public.cohort_membership (person_id, id) include (cohort_id);

drop index if exists cohort_membership_status_idx;
drop index if exists tenant_membership_status_idx;

create index if not exists couple_man_active_idx on couple (man_id) where status = 'active';
create index if not exists couple_woman_active_idx on couple (woman_id) where status = 'active';

create index if not exists couple_woman_active_lookup on public.couple (woman_id, since, until, id) include (man_id, status) where status='active';
create index if not exists couple_man_active_lookup on public.couple (man_id, since, until, id) include (woman_id, status) where status='active';

create index if not exists tenant_membership_tenant_active_idx on tenant_membership (tenant_id) where status = 'active';

create index if not exists cohort_membership_active_by_person on cohort_membership (person_id) include (cohort_id) where status = 'active';
create index if not exists tenant_membership_active_by_person on tenant_membership (person_id) include (tenant_id) where status = 'active';
create index if not exists tenant_trainer_active_by_person on tenant_trainer (person_id) include (tenant_id) where status = 'active';
create index if not exists tenant_administrator_active_by_person on tenant_administrator (person_id) include (tenant_id) where status = 'active';

--!include views/auth_details.sql
--!include policies/couple.sql
--!include policies/person.sql
