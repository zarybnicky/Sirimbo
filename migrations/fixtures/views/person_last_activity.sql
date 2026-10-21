-- Owner privileges allow summaries of private records without granting access to
-- account links, card codes, or invitations. Keep caller/tenant checks here.
create or replace view person_last_activity
with (security_barrier = false) as
select
  (select current_tenant_id()) as tenant_id,
  p.id as person_id,
  web.web_at,
  card.card_entry_at,
  training.training_at,
  competition.competition_on
from person p
left join lateral (
  select max(u.last_active_at) as web_at
  from user_proxy up
  join users u on u.id = up.user_id
  where up.person_id = p.id and up.status = 'active'
) web on true
left join lateral (
  select e.occurred_at as card_entry_at
  from access_event e
  where e.tenant_id = (select current_tenant_id())
    and e.person_id = p.id and e.allowed and e.occurred_at <= now()
  order by e.occurred_at desc
  limit 1
) card on true
left join lateral (
  select max(e.since) as training_at
  from event_instance_registration r
  join event_instance e on e.id = r.instance_id and e.tenant_id = r.tenant_id
  where r.person_id = p.id and r.tenant_id = (select current_tenant_id())
    and r.registration_status = 'active'
    and (r.status = 'attended' or e.type = 'lesson')
    and not e.is_cancelled and e.since <= now()
    and e.type in ('lesson', 'group', 'camp')
) training on true
left join lateral (
  select max(c.start_date) as competition_on
  from (values ('csts', p.csts_id::bigint), ('wdsf', p.wdsf_id::bigint)) l(federation, external_id)
  join federated.person fp on (fp.federation, fp.external_id) = (l.federation, l.external_id)
  join federated.competitor_component cc on cc.person_id = fp.id
  join federated.competition_result r on r.competitor_id = cc.competitor_id
  join federated.competition c on c.id = r.competition_id
  where l.external_id <> 0 and c.start_date <= current_date
) competition on true
where (select pg_has_role(current_user, 'trainer', 'member'))
  and (
    exists (select from current_tenant_membership m where m.person_id = p.id)
    or exists (select from current_tenant_trainer t where t.person_id = p.id)
    or exists (select from current_tenant_administrator a where a.person_id = p.id)
    or exists (select from current_cohort_membership cm where cm.person_id = p.id)
  );

comment on view person_last_activity is '@primaryKey person_id
@unique tenant_id,person_id
@foreignKey (person_id) references public.person (id)|@foreignFieldName lastActivity
@behavior -query:resource:single -query:resource:list -query:resource:connection -insert -update -delete';
comment on column person_last_activity.tenant_id is '@notNull';
comment on column person_last_activity.person_id is '@notNull';

revoke all on person_last_activity from public;
grant select on person_last_activity to anonymous;
