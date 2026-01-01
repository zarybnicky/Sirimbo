
delete from event_instance where until = since;
delete from event where not exists (select 1 from event_instance ei where ei.event_id = event.id);

update event_instance set is_cancelled = false where is_cancelled is null;

alter table event_instance
  alter is_cancelled set default false,
  alter is_cancelled set not null,
  drop constraint if exists event_instance_until_gt_since,
  add constraint event_instance_until_gt_since check (until > since);

alter table tenant_administrator alter until drop default, alter until drop not null;
alter table tenant_trainer alter until drop default, alter until drop not null;
alter table tenant_membership alter until drop default, alter until drop not null;
alter table cohort_membership alter until drop default, alter until drop not null;
alter table couple alter until drop default, alter until drop not null;

update tenant_administrator set until = null where until = 'infinity'::timestamptz;
update tenant_trainer set until = null where until = 'infinity'::timestamptz;
update tenant_membership set until = null where until = 'infinity'::timestamptz;
update cohort_membership set until = null where until = 'infinity'::timestamptz;
update couple set until = null where until = 'infinity'::timestamptz;

alter table user_proxy
  alter since drop NOT NULL,
  alter until drop NOT NULL,
  alter until drop DEFAULT;
update user_proxy set since = null where since = '-infinity'::timestamptz;
update user_proxy set until = null where until = 'infinity'::timestamptz;

drop function if exists archived_announcements;
drop function if exists my_announcements(boolean, boolean);
drop function if exists login;

--!include functions/event_instance_trainers.sql
--!include functions/event_instance_approx_price.sql
--!include functions/my_announcements.sql
--!include functions/otp_login.sql
--!include functions/register_using_invitation.sql
--!include functions/register_without_invitation.sql
--!include functions/log_in_as.sql
--!include functions/login.sql
--!include functions/set_lesson_demand.sql
