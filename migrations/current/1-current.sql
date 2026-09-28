do $$
begin
  if exists (
    select 1 from information_schema.columns
    where table_schema = 'public' and table_name = 'tenant_location' and column_name = 'is_public'
  ) and not exists (
    select 1 from information_schema.columns
    where table_schema = 'public' and table_name = 'tenant_location' and column_name = 'show_in_lists'
  ) then
    alter table tenant_location rename column is_public to show_in_lists;
  end if;
end;
$$;

alter table tenant_location add column if not exists show_in_lists boolean not null default false;
alter table tenant_location alter column show_in_lists set default false;
alter table tenant_location add column if not exists is_public boolean generated always as (show_in_lists) stored;
alter table tenant_location add column if not exists long_name text;
alter table tenant_location add column if not exists latitude double precision;
alter table tenant_location add column if not exists longitude double precision;

comment on table tenant_location is '@simpleCollections only
@behavior -query:resource:list -query:resource:connection -queryField:resource:connection';
grant all on tenant_location to anonymous;

update tenant_location
set long_name = coalesce(long_name, name),
    name = 'ZŠ Holečkova',
    latitude = coalesce(latitude, 49.57963),
    longitude = coalesce(longitude, 17.2495939),
    address = coalesce(address, row('Holečkova', '10', '', '', 'Olomouc', '', '779 00')::address_domain),
    description = case when description = '' then '<p>Vchod brankou u zastávky Povel – škola.</p><p><a href="https://www.zsholeckova.cz/">Web školy</a></p>' else description end
where tenant_id = 1 and id = 1 and name = 'ZŠ Holečkova';

update tenant_location
set long_name = coalesce(long_name, name),
    name = 'SGO',
    latitude = coalesce(latitude, 49.5949),
    longitude = coalesce(longitude, 17.2634),
    address = coalesce(address, row('Jiřího z Poděbrad', '13', '', '', 'Olomouc', '', '779 00')::address_domain),
    description = case when description = '' then '<p>Vchod brankou z ulice U reálky.</p><p><a href="https://www.sgo.cz/">Web školy</a></p>' else description end
where tenant_id = 1 and id = 4 and name = 'SGO';

--!include functions/upsert_location.sql
--!include functions/visible_file_ids.sql
