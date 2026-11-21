grant connect on database :DATABASE_NAME to :DATABASE_OWNER;
grant all on database :DATABASE_NAME to :DATABASE_OWNER;
grant temp on database :DATABASE_NAME to public;
alter user :DATABASE_OWNER with superuser;
alter schema public owner to :DATABASE_OWNER;

create extension if not exists plpgsql with schema pg_catalog;
create extension if not exists pgcrypto with schema public;
create extension if not exists http;
create extension if not exists citext;
create extension if not exists hypopg;

drop extension if exists plpgsql_check;
create extension plpgsql_check;

do $$
BEGIN
  IF NOT EXISTS (SELECT FROM pg_catalog.pg_roles WHERE rolname='anonymous') THEN
    CREATE ROLE anonymous;
  END IF;
  IF NOT EXISTS (SELECT FROM pg_catalog.pg_roles WHERE rolname='member') THEN
    CREATE ROLE member;
  END IF;
  IF NOT EXISTS (SELECT FROM pg_catalog.pg_roles WHERE rolname='administrator') THEN
    CREATE ROLE administrator;
  END IF;
END
$$;

revoke all on schema public from public;
alter default privileges revoke all on sequences from public;
alter default privileges revoke all on functions from public;

grant all on schema public to postgres;
grant all on schema public to :DATABASE_OWNER;

grant anonymous to member, administrator;
grant member to administrator;

grant all on schema public to anonymous;
alter default privileges for role :DATABASE_OWNER in schema public
  grant usage, select on sequences to anonymous;
alter default privileges in schema public grant usage, select on sequences to anonymous;
