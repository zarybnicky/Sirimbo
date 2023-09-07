drop function if exists users_has_valid_payment;
drop table if exists app_private.parameters;

alter table users
  drop column if exists u_poznamky,
  drop column if exists u_system,
  drop column if exists u_member_since,
  drop column if exists u_member_until,
  drop column if exists u_gdpr_signed_at,
  drop column if exists u_nationality;


drop view if exists app_private.auth_details;
create or replace view app_private.auth_details as
  SELECT
    person.id as person_id,
    array_agg(couple.id) as couple_ids,
    array_agg(cohort_id) as cohort_memberships,
    array_remove(array_agg(tenant_membership.tenant_id), null) tenant_memberships,
    array_remove(array_agg(tenant_trainer.tenant_id), null) tenant_trainers,
    array_remove(array_agg(tenant_administrator.tenant_id), null) tenant_administrators
  from person
  left join couple on (person.id=couple.man_id or person.id=couple.woman_id) and now() <@ couple.active_range
  left join cohort_membership on person.id=cohort_membership.person_id and now() <@ cohort_membership.active_range
  left join tenant_membership on person.id=tenant_membership.person_id and now() <@ tenant_membership.active_range
  left join tenant_trainer on person.id=tenant_trainer.person_id and now() <@ tenant_trainer.active_range
  left join tenant_administrator on person.id=tenant_administrator.person_id and now() <@ tenant_administrator.active_range
  group by person.id;
