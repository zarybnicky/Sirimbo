do $$
begin
  if not exists (select * from pg_type where typcategory='E' and typname = 'relationship_status') then
    create type relationship_status as enum (
      'pending',
      'active',
      'expired'
    );
  end if;
end
$$;
COMMENT ON TABLE public.posting IS E'@omit create,update,delete
@simpleCollections both';

alter table user_proxy add column if not exists status relationship_status not null default 'active';
alter table couple add column if not exists status relationship_status not null default 'active';
alter table cohort_membership add column if not exists status relationship_status not null default 'active';
alter table tenant_membership add column if not exists status relationship_status not null default 'active';
alter table tenant_trainer add column if not exists status relationship_status not null default 'active';
alter table tenant_administrator add column if not exists status relationship_status not null default 'active';

drop function if exists user_proxy_active;
drop function if exists couple_active;
drop function if exists cohort_membership_active;
drop function if exists tenant_membership_active;
drop function if exists tenant_trainer_active;
drop function if exists tenant_administrator_active;
alter table user_proxy add column if not exists active boolean not null generated always as (status = 'active') stored;
alter table couple add column if not exists active boolean not null generated always as (status = 'active') stored;
alter table cohort_membership add column if not exists active boolean not null generated always as (status = 'active') stored;
alter table tenant_membership add column if not exists active boolean not null generated always as (status = 'active') stored;
alter table tenant_trainer add column if not exists active boolean not null generated always as (status = 'active') stored;
alter table tenant_administrator add column if not exists active boolean not null generated always as (status = 'active') stored;

create index if not exists user_proxy_status_idx on user_proxy (status);
create index if not exists couple_status_idx on couple (status);
create index if not exists cohort_membership_status_idx on cohort_membership (status);
create index if not exists tenant_membership_status_idx on tenant_membership (status);
create index if not exists tenant_trainer_status_idx on tenant_trainer (status);
create index if not exists tenant_administrator_status_idx on tenant_administrator (status);

create index if not exists user_proxy_active_idx on user_proxy (active);
create index if not exists couple_active_idx on couple (active);
create index if not exists cohort_membership_active_idx on cohort_membership (active);
create index if not exists tenant_membership_active_idx on tenant_membership (active);
create index if not exists tenant_trainer_active_idx on tenant_trainer (active);
create index if not exists tenant_administrator_active_idx on tenant_administrator (active);

create or replace function app_private.cron_update_memberships() returns void language sql as $$
  update user_proxy set status = 'active' where now() <@ active_range and status <> 'active';
  update user_proxy set status = 'pending' where now() < since and status <> 'pending';
  update user_proxy set status = 'expired' where now() > until and status <> 'expired';

  update couple set status = 'active' where now() <@ active_range and status <> 'active';
  update couple set status = 'pending' where now() < since and status <> 'pending';
  update couple set status = 'expired' where now() > until and status <> 'expired';

  update cohort_membership set status = 'active' where now() <@ active_range and status <> 'active';
  update cohort_membership set status = 'pending' where now() < since and status <> 'pending';
  update cohort_membership set status = 'expired' where now() > until and status <> 'expired';

  update tenant_membership set status = 'active' where now() <@ active_range and status <> 'active';
  update tenant_membership set status = 'pending' where now() < since and status <> 'pending';
  update tenant_membership set status = 'expired' where now() > until and status <> 'expired';

  update tenant_trainer set status = 'active' where now() <@ active_range and status <> 'active';
  update tenant_trainer set status = 'pending' where now() < since and status <> 'pending';
  update tenant_trainer set status = 'expired' where now() > until and status <> 'expired';

  update tenant_administrator set status = 'active' where now() <@ active_range and status <> 'active';
  update tenant_administrator set status = 'pending' where now() < since and status <> 'pending';
  update tenant_administrator set status = 'expired' where now() > until and status <> 'expired';
$$;
grant all on function app_private.cron_update_memberships to administrator;

do $$
declare
  id bigint;
begin
  if exists (select 1 from pg_extension where extname='pg_cron') then
    select jobid into id from cron.job where jobname = 'refresh auth_details';
    if found then
      perform cron.unschedule(id);
    end if;

    perform cron.schedule('update memberships', '59 seconds', 'select app_private.cron_update_memberships();');
  end if;
end
$$;

create or replace function app_private.tg_tenant_membership__on_status() returns trigger language plpgsql as $$
begin
  if NEW.status = 'expired' then
    update cohort_membership set status = 'expired', until = NEW.until where cohort_membership.person_id = NEW.person_id;
  end if;
  return NEW;
end;
$$;
select verify_function('app_private.tg_tenant_membership__on_status', 'tenant_membership');

CREATE or replace TRIGGER _500_on_status
  after UPDATE ON tenant_membership
  FOR EACH row
  WHEN (OLD.status IS DISTINCT FROM NEW.status)
  EXECUTE PROCEDURE app_private.tg_tenant_membership__on_status();

create or replace function app_private.tg_cohort_membership__on_status() returns trigger language plpgsql as $$
begin
  if NEW.status = 'expired' and (TG_OP = 'INSERT' or OLD.status <> NEW.status) then
    -- TODO: remove event_registrations for future events
    -- remove event_attendance for ongoing events
  elsif NEW.status = 'active' and (TG_OP = 'INSERT' or OLD.status <> NEW.status) then
    -- add payments
    -- add event_registrations to cohort events
  end if;
  return NEW;
end;
$$;
select verify_function('app_private.tg_cohort_membership__on_status', 'cohort_membership');

CREATE or replace TRIGGER _500_on_status
  after insert or UPDATE ON cohort_membership
  FOR EACH row
  EXECUTE PROCEDURE app_private.tg_cohort_membership__on_status();
