alter table event drop column if exists since, drop column if exists until;

drop function if exists sticky_announcements;

grant usage on schema app_private to anonymous;

--!include functions/event_instance_approx_price.sql
--!include functions/create_jwt_token.sql

DO $do$ BEGIN
  IF NOT EXISTS (SELECT FROM pg_catalog.pg_roles WHERE rolname='system_admin') THEN
    CREATE ROLE system_admin;
  END IF;
END $do$;
grant administrator to system_admin;
