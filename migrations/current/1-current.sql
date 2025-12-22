CREATE INDEX IF NOT EXISTS json_response_frontier_fetched_desc_idx
  ON crawler.json_response (frontier_id, fetched_at DESC);

CREATE INDEX IF NOT EXISTS html_response_frontier_fetched_desc_idx
  ON crawler.html_response (frontier_id, fetched_at DESC);

CREATE INDEX IF NOT EXISTS frontier_process_pending_ok_gone_pick_idx
  ON crawler.frontier (last_fetched_at, discovered_at, id)
  WHERE process_status = 'pending'
    AND fetch_status IN ('ok','gone');

alter table user_proxy alter column active_range set expression as (tstzrange(since, until, '[)'::text));
alter table tenant_administrator alter column active_range set expression as (tstzrange(since, until, '[)'::text));
alter table tenant_trainer alter column active_range set expression as (tstzrange(since, until, '[)'::text));
alter table tenant_membership alter column active_range set expression as (tstzrange(since, until, '[)'::text));
alter table cohort_membership alter column active_range set expression as (tstzrange(since, until, '[)'::text));
alter table couple alter column active_range set expression as (tstzrange(since, until, '[)'::text));
alter table accounting_period alter column range set expression as (tstzrange(since, until, '[)'::text));

comment on column user_proxy.active is '@omit';
comment on column tenant_administrator.active is '@omit';
comment on column tenant_trainer.active is '@omit';
comment on column tenant_membership.active is '@omit';
comment on column cohort_membership.active is '@omit';
comment on column couple.active is '@omit';

alter table public.user_proxy
  drop constraint if exists user_proxy_no_overlap,
  add constraint user_proxy_no_overlap exclude using gist (user_id with =, person_id with =, active_range with &&);

alter table public.tenant_administrator
  drop constraint if exists tenant_administrator_no_overlap,
  add constraint tenant_administrator_no_overlap exclude using gist (tenant_id with =, person_id with =, active_range with &&);

alter table public.tenant_trainer
  drop constraint if exists tenant_trainer_no_overlap,
  add constraint tenant_trainer_no_overlap exclude using gist (tenant_id with =, person_id with =, active_range with &&);

alter table public.tenant_membership
  drop constraint if exists tenant_membership_no_overlap,
  add constraint tenant_membership_no_overlap exclude using gist (tenant_id with =, person_id with =, active_range with &&);

alter table public.cohort_membership
  drop constraint if exists cohort_membership_no_overlap,
  add constraint cohort_membership_no_overlap exclude using gist (cohort_id with =, person_id with =, active_range with &&);

alter table public.couple
  drop constraint if exists couple_no_overlap,
  add constraint couple_no_overlap exclude using gist (man_id with =, woman_id with =, active_range with &&);

alter table public.accounting_period
  drop constraint if exists accounting_period_no_overlap,
  add constraint accounting_period_no_overlap exclude using gist (tenant_id with =, range with &&);
