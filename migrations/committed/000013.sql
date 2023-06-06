--! Previous: sha1:e445ed77d4b629e659776fec0b5e9613ecaa6c6d
--! Hash: sha1:03f3dafd03b178a542f466753f08f7bb2faf0128

-- Write your migration here

alter table tenant add column if not exists origins text[] not null default array[]::text[];

create or replace function current_tenant_id() returns bigint as $$
  select nullif(current_setting('jwt.claims.tenant_id', '1')::bigint, 1);
$$ language sql stable;
grant execute on function current_tenant_id to anonymous;
