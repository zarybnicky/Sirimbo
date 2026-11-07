
drop function if exists person_is_admin(person);
drop function if exists person_is_trainer(person);
drop function if exists person_is_member(person);
drop function if exists person_cohort_ids(person);
drop function if exists payment_debtor_is_unpaid(payment_debtor);
drop function if exists account_balance(account);
drop function if exists tenant_membership_tenant_name(tenant_membership);
drop function if exists tenant_trainer_tenant_name(tenant_trainer);
drop function if exists tenant_administrator_tenant_name(tenant_administrator);
drop function if exists access_credential_is_allowed(access_credential);

alter table crawler.frontier add column if not exists reference_date date;

create table if not exists crawler.refetch_policy (
  federation text not null,
  kind text not null,
  reference_offset interval,
  fetch_interval interval not null check (fetch_interval > interval '0 seconds'),
  unique nulls not distinct (federation, kind, reference_offset)
);

comment on table crawler.refetch_policy is '@behavior -*';
grant select on crawler.refetch_policy to system_admin;

alter table crawler.refetch_policy enable row level security;
drop policy if exists system_admin_read on crawler.refetch_policy;
create policy system_admin_read on crawler.refetch_policy for select to system_admin using (true);

--!include data/crawler_refetch_policy.sql

--!include functions/crawler_refetch.sql

update crawler.frontier f set reference_date = coalesce(c.end_date, c.start_date)
from federated.competition c
where f.reference_date is null and c.federation = f.federation and c.external_id = f.key
  and ((f.federation = 'wdsf' and f.kind in ('competition', 'participantIndex', 'officialIndex'))
    or (f.federation = 'csts' and f.kind = 'competitionResults'));

update crawler.frontier f set reference_date = coalesce(e.end_date, e.start_date)
from federated.event e
where f.reference_date is null and e.federation = f.federation and e.external_id = f.key
  and f.federation = 'csts' and f.kind in ('event', 'eventCompetitors');

update crawler.frontier set reference_date = (key || '-01')::date + interval '1 month -1 day'
where reference_date is null and key ~ '^\d{4}-\d{2}$'
  and ((federation = 'wdsf' and kind = 'competitionIndex')
    or (federation = 'csts' and kind in ('eventIndex', 'competitionEventIndex')));

with participants as (
  select p->>'id' as key, f.reference_date
  from crawler.frontier f
  join crawler.json_response r on r.id = f.last_successful_response_id
  join crawler.json_response_cache rc on rc.content_hash = r.content_hash
  cross join lateral jsonb_array_elements(rc.content) p
  where f.federation = 'wdsf' and f.kind = 'participantIndex' and f.reference_date is not null
)
update crawler.frontier f set reference_date = p.reference_date
from participants p
where f.federation = 'wdsf' and f.kind = 'participant' and f.key = p.key and f.reference_date is null;

update crawler.frontier f set reference_date = (rc.content->'entity'->>'date')::date
from crawler.json_response r join crawler.json_response_cache rc on rc.content_hash = r.content_hash
where f.federation = 'csts' and f.kind = 'ranklist' and f.reference_date is null
  and r.id = f.last_successful_response_id;

alter type federated.person_license_status add value if not exists 'temporary';
alter type federated.person_license_discipline add value if not exists 'esports';

drop function if exists event_trainer_tenant_trainer;
drop function if exists account_assets;
drop function if exists account_liabilities;
drop function if exists event_trainers;

do $$
begin
  if not exists (
    select 1 from pg_attribute
    where attrelid = 'public.event'::regclass
      and attname = 'registration_period' and not attisdropped
  ) then
    alter table event add column registration_period tstzrange default '(,)'::tstzrange;
    update event set registration_period = null where is_locked;
  end if;
end;
$$;

comment on column event.registration_period is null;
comment on column event.is_locked is '@behavior -*';

--!include functions/save_events.sql
--!include functions/tg_event__refresh_manager_person_ids.sql
--!include functions/event_registration_info.sql
--!include functions/set_lesson_request.sql
--!include functions/set_event_registration.sql
--!include views/crawler_frontier_fetch_job.sql
