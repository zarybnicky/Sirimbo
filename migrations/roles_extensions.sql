GRANT CONNECT ON DATABASE :DATABASE_NAME TO :DATABASE_OWNER;
GRANT ALL ON DATABASE :DATABASE_NAME TO :DATABASE_OWNER;
ALTER SCHEMA public OWNER TO :DATABASE_OWNER;

CREATE EXTENSION IF NOT EXISTS plpgsql WITH SCHEMA pg_catalog;
CREATE EXTENSION IF NOT EXISTS pgcrypto WITH SCHEMA public;

revoke all on schema public from public;
alter default privileges revoke all on sequences from public;
alter default privileges revoke all on functions from public;

grant all on schema public to postgres;
grant all on schema public to :DATABASE_OWNER;

grant usage on schema public to :DATABASE_VISITOR;
alter default privileges in schema public grant usage, select on sequences to :DATABASE_VISITOR;
alter default privileges in schema public grant execute on functions to :DATABASE_VISITOR;