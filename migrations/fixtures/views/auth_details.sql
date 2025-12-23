create or replace view auth_details_view as
SELECT
  person.id as person_id,
  array_remove(array_agg(c_woman.id) || array_agg(c_man.id), null) as couple_ids,
  array_remove(array_agg(cohort_id), null) as cohort_memberships,
  array_remove(array_agg(tenant_membership.tenant_id), null) tenant_memberships,
  array_remove(array_agg(tenant_trainer.tenant_id), null) tenant_trainers,
  array_remove(array_agg(tenant_administrator.tenant_id), null) tenant_administrators,
  array_remove(array_agg(tenant_administrator.tenant_id) || array_agg(tenant_trainer.tenant_id) || array_agg(tenant_membership.tenant_id), null) allowed_tenants
from person
  left join couple c_woman on person.id=c_woman.woman_id and c_woman.status='active'
  left join couple c_man on person.id=c_man.man_id and c_man.status='active'
  left join cohort_membership on person.id=cohort_membership.person_id and cohort_membership.status='active'
  left join tenant_membership on person.id=tenant_membership.person_id and tenant_membership.status='active'
  left join tenant_trainer on person.id=tenant_trainer.person_id and tenant_trainer.status='active'
  left join tenant_administrator on person.id=tenant_administrator.person_id and tenant_administrator.status='active'
group by person.id;

grant all on auth_details_view to anonymous;
comment on view auth_details_view is E'@omit';
