--! Previous: sha1:dff0fe4ee3a3d0e0374530cb131e6775ad6009f7
--! Hash: sha1:85b7bda0687a2fc952bf138bf6cee654e3200610

--! split: 1-current.sql
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
alter table tenant_location add column if not exists ordering integer not null default 1;

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

--! Included functions/upsert_location.sql
drop function if exists upsert_location;
drop type if exists location_details_input;

create type location_details_input as (
  id bigint,
  name text,
  long_name text,
  description text,
  address address_domain,
  show_in_lists boolean,
  ordering integer,
  latitude double precision,
  longitude double precision
);

create or replace function upsert_location(
  details location_details_input,
  image_ids bigint[] default null,
  cover_image_id bigint default null
)
returns tenant_location
language plpgsql
as $$
declare
  result tenant_location;
begin
  if details.id is null then
    insert into tenant_location (
      name,
      long_name,
      description,
      address,
      show_in_lists,
      ordering,
      latitude,
      longitude,
      cover_image_id
    )
    values (
      details.name,
      nullif(nullif(btrim(details.long_name), ''), details.name),
      coalesce(details.description, ''),
      details.address,
      coalesce(details.show_in_lists, false),
      coalesce(details.ordering, 1),
      details.latitude,
      details.longitude,
      cover_image_id
    )
    returning * into result;
  else
    update tenant_location
    set name = details.name,
        long_name = nullif(nullif(btrim(details.long_name), ''), details.name),
        description = coalesce(details.description, ''),
        address = details.address,
        show_in_lists = coalesce(details.show_in_lists, tenant_location.show_in_lists),
        ordering = coalesce(details.ordering, tenant_location.ordering),
        latitude = details.latitude,
        longitude = details.longitude,
        cover_image_id = upsert_location.cover_image_id
    where id = details.id
    returning * into result;

    if not found then
      raise exception 'Location with id % not found', details.id;
    end if;
  end if;

  if image_ids is not null then
    select coalesce(array_agg(id), '{}'::bigint[])
    into image_ids
    from file
    where id = any(image_ids)
      and tenant_id = result.tenant_id
      and uploaded_at is not null
      and content_type like 'image/%';

    delete from tenant_location_image image
    where image.tenant_id = result.tenant_id
      and image.location_id = result.id
      and image.file_id <> all(image_ids);

    insert into tenant_location_image (tenant_id, location_id, file_id)
    select result.tenant_id, result.id, file_id
    from unnest(image_ids) input(file_id)
    on conflict do nothing;
  end if;

  return result;
end;
$$;

revoke all on function upsert_location from anonymous;
grant all on function upsert_location to administrator;
--! EndIncluded functions/upsert_location.sql
--! Included functions/visible_file_ids.sql
create or replace function app_private.visible_file_ids()
  returns setof bigint
  language sql stable
  security definer
  set search_path = pg_catalog, public, pg_temp
as $$
  select id
  from file
  where tenant_id = (select current_tenant_id())
    and is_public

  union

  select f.file_id
  from announcement_attachment f
  join app_private.visible_announcement_ids() a(id) on a.id = f.announcement_id
  where f.tenant_id = (select current_tenant_id())

  union

  select f.file_id
  from article_attachment f
  join aktuality a on id = f.aktuality_id and a.tenant_id = f.tenant_id
  where a.tenant_id = (select current_tenant_id())
    and a.is_visible

  union

  select image.file_id
  from tenant_location_image image
  join tenant_location location
    on location.tenant_id = image.tenant_id
    and location.id = image.location_id
  where image.tenant_id = (select current_tenant_id())

  union

  select cover_image_id
  from tenant_location
  where tenant_id = (select current_tenant_id())
    and cover_image_id is not null;
$$;

grant execute on function app_private.visible_file_ids() to anonymous;
--! EndIncluded functions/visible_file_ids.sql
--! Included functions/confirm_membership_application.sql
drop function if exists confirm_membership_application(bigint);

create or replace function confirm_membership_application(
  application_id bigint,
  is_member boolean default true,
  is_trainer boolean default false,
  is_admin boolean default false,
  join_date timestamptz default now(),
  cohort_ids bigint[] default array[]::bigint[]
)
  returns person
  language sql
as $$
  with application as materialized (
    select *
    from membership_application
    where id = application_id and status = 'sent'
    for update
  ), t_person as (
    insert into person (
      first_name, last_name, gender, birth_date, nationality, tax_identification_number,
      national_id_number, csts_id, wdsf_id, prefix_title, suffix_title, bio, email, phone,
      note
    )
    select
      first_name, last_name, gender, birth_date, nationality, tax_identification_number,
      national_id_number, csts_id, wdsf_id,
      coalesce(prefix_title, ''), coalesce(suffix_title, ''), coalesce(bio, ''),
      email, phone, coalesce(note, '')
    from application
    returning *
  ), appl as (
    update membership_application
    set status = 'approved'
    where id = (select id from application)
  ), member as (
    insert into tenant_membership (tenant_id, person_id, since)
    select current_tenant_id(), id, join_date from t_person where is_member
  ), trainer as (
    insert into tenant_trainer (tenant_id, person_id, since)
    select current_tenant_id(), id, join_date from t_person where is_trainer
  ), administrator as (
    insert into tenant_administrator (tenant_id, person_id, since)
    select current_tenant_id(), id, join_date from t_person where is_admin
  ), cohorts as (
    insert into cohort_membership (cohort_id, person_id, since)
    select cohort_id, t_person.id, join_date
    from t_person
    cross join unnest(coalesce(cohort_ids, array[]::bigint[])) selected(cohort_id)
  ), proxy as (
    insert into user_proxy (person_id, user_id)
    select t_person.id, application.created_by
    from t_person cross join application
  )
  select * from t_person;
$$;

grant all on function confirm_membership_application to administrator;
--! EndIncluded functions/confirm_membership_application.sql

alter table event_instance
  drop constraint event_instance_parent_id_fkey,
  add constraint event_instance_parent_id_fkey
    foreign key (tenant_id, parent_id)
    references event_instance (tenant_id, id)
    on update cascade;

comment on constraint event_instance_parent_id_fkey on event_instance
  is E'@fieldName parent\n@foreignFieldName childEventInstances';
