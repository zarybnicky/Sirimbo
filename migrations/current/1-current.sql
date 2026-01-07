alter table event
  drop column if exists since,
  drop column if exists until;

drop index if exists u_jmeno;
drop index if exists u_prijmeni;
drop index if exists u_confirmed;

alter table users drop column if exists u_confirmed;

do $$
begin
  if exists (select * from information_schema.columns where table_name = 'users' and column_name = 'u_created_at') then
    update users set u_created_at = created_at where created_at is null;
    alter table users drop column created_at;
    alter table users rename column u_created_at to created_at;
  end if;
end;
$$;

drop function if exists sticky_announcements;

grant usage on schema app_private to anonymous;

DO $do$ BEGIN
  IF NOT EXISTS (SELECT FROM pg_catalog.pg_roles WHERE rolname='system_admin') THEN
    CREATE ROLE system_admin;
  END IF;
END $do$;
grant administrator to system_admin;

--!include functions/event_instance_approx_price.sql
--!include functions/create_jwt_token.sql
--!include functions/cron_update_memberships.sql
