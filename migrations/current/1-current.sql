--!include functions/reconcile_event_instance_cohort_registrations.sql
--!include functions/save_events.sql
--!include policies/access_credential.sql

alter table crawler.json_response add column if not exists sys_period tstzrange;
alter table crawler.json_response add column if not exists last_observed_at timestamptz;

alter table crawler.json_response drop constraint if exists json_response_observation_check;
alter table crawler.json_response add constraint json_response_observation_check check (
  (sys_period is null and last_observed_at is null)
  or (
    sys_period is not null and last_observed_at is not null
    and not isempty(sys_period) and not lower_inf(sys_period)
    and lower_inc(sys_period) and not upper_inc(sys_period)
    and lower(sys_period) = fetched_at
    and sys_period @> last_observed_at
  )
);

create unique index if not exists json_response_current_frontier_idx
  on crawler.json_response (frontier_id) where upper_inf(sys_period);

--!include functions/crawler_store_response.sql
--!include views/crawler_frontier_failure.sql
