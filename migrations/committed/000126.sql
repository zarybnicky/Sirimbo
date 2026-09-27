--! Previous: sha1:081f2239dfcd68c8a890b91ca2a07d0aae36db9d
--! Hash: sha1:790e4174be3c995f46e169e95568b646c8d108e3

--! split: 1-current.sql
do $$
begin
  if not exists (
    select 1 from pg_type t join pg_namespace n on n.oid = t.typnamespace
    where n.nspname = 'public' and t.typname = 'security_event_kind'
  ) then
    create type security_event_kind as enum (
      'login_succeeded', 'login_failed', 'registration', 'impersonation',
      'password_reset_requested', 'password_changed', 'invitation_accepted',
      'person_linked', 'person_unlinked',
      'membership_granted', 'membership_revoked',
      'trainer_granted', 'trainer_revoked',
      'administrator_granted', 'administrator_revoked',
      'access_credential_issued', 'access_credential_ended'
    );
  end if;

  if not exists (
    select 1 from pg_type t join pg_namespace n on n.oid = t.typnamespace
    where n.nspname = 'public' and t.typname = 'security_event_method'
  ) then
    create type security_event_method as enum ('password', 'otp', 'manual', 'scheduled');
  end if;
end;
$$;

alter table security_event drop constraint if exists security_event_method_check;
alter table security_event
  alter column kind type security_event_kind using kind::security_event_kind,
  alter column method type security_event_method using method::security_event_method;

create table if not exists tenant_location_image (
  tenant_id bigint not null default current_tenant_id(),
  location_id bigint not null,
  file_id bigint not null,
  primary key (tenant_id, location_id, file_id),
  constraint tenant_location_image_location_fk
    foreign key (tenant_id, location_id) references tenant_location(tenant_id, id) on delete cascade,
  constraint tenant_location_image_file_fk
    foreign key (tenant_id, file_id) references file(tenant_id, id) on delete cascade
);

alter table tenant_location_image
  drop column if exists position,
  drop column if exists is_cover;

alter table tenant_location
  add column if not exists cover_image_id bigint;

do $$
begin
  if not exists (
    select 1
    from pg_constraint
    where conname = 'tenant_location_cover_image_fk'
      and conrelid = 'tenant_location'::regclass
  ) then
    alter table tenant_location
      add constraint tenant_location_cover_image_fk
      foreign key (tenant_id, cover_image_id)
      references file (tenant_id, id)
      on delete set null (cover_image_id);
  end if;
end;
$$;

create index if not exists tenant_location_image_file_idx
  on tenant_location_image (tenant_id, file_id);
create index if not exists tenant_location_cover_image_idx
  on tenant_location (tenant_id, cover_image_id)
  where cover_image_id is not null;

comment on table tenant_location is '@simpleCollections only
@behavior -query:resource:list -query:resource:connection -queryField:resource:connection';
comment on table tenant_location_image is '@omit create,update,delete
@simpleCollections only';
comment on constraint tenant_location_image_location_fk on tenant_location_image
  is '@fieldName location
@foreignFieldName images';
comment on constraint tenant_location_image_file_fk on tenant_location_image
  is '@fieldName file
@foreignFieldName locationImages';
comment on constraint tenant_location_cover_image_fk on tenant_location
  is '@fieldName coverImage
@behavior -manyRelation:resource:list -manyRelation:resource:connection';

grant all on table tenant_location_image to anonymous;
alter table tenant_location_image enable row level security;

select app_private.drop_policies('public.tenant_location_image');
create policy current_tenant on tenant_location_image as restrictive using (tenant_id = (select current_tenant_id()));
create policy admin_all on tenant_location_image to administrator using (true);
create policy public_view on tenant_location_image for select using (true);

--! Included functions/security_events.sql
create or replace function app_private.tg_relationship__status()
  returns trigger
  language plpgsql
as $$
begin
  new.status := app_private.relationship_status_next(
    now(), tstzrange(new.since, new.until, '[)'), new.status
  );
  return new;
end;
$$;

create or replace function app_private.tg_security_event__range()
  returns trigger
  language plpgsql security definer
  set search_path to pg_catalog, public, pg_temp
as $$
begin
  if tg_op = 'DELETE' then
    if old.status = 'active' then
      insert into security_event (person_id, kind, method)
      values (old.person_id, tg_argv[1]::security_event_kind, 'manual');
    end if;
    return old;
  end if;

  if tg_op = 'INSERT' and new.status = 'active' then
    insert into security_event (person_id, kind, method, effective_at)
    values (new.person_id, tg_argv[0]::security_event_kind, 'manual', new.since);
  elsif tg_op = 'UPDATE'
     and new.status is distinct from old.status
     and new.status in ('active', 'expired') then
    insert into security_event (person_id, kind, method, effective_at)
    values (
      new.person_id,
      (case when new.status = 'active' then tg_argv[0] else tg_argv[1] end)::security_event_kind,
      (case when current_user_id() is null then 'scheduled' else 'manual' end)::security_event_method,
      case when new.status = 'active' then new.since else new.until end
    );
  end if;
  return new;
end;
$$;

create or replace function app_private.tg_security_event__credential()
  returns trigger
  language plpgsql security definer
  set search_path to pg_catalog, public, pg_temp
as $$
begin
  if tg_op = 'DELETE' then
    if old.status = 'active' then
      insert into security_event (person_id, kind, method)
      values (old.person_id, 'access_credential_ended', 'manual');
    end if;
    return old;
  end if;

  if tg_op = 'INSERT' and new.status = 'active' then
    insert into security_event (person_id, kind, method, effective_at)
    values (new.person_id, 'access_credential_issued', 'manual', new.since);
  elsif tg_op = 'UPDATE'
     and new.status is distinct from old.status
     and new.status in ('active', 'expired') then
    insert into security_event (person_id, kind, method, effective_at)
    values (
      new.person_id,
      (case when new.status = 'active' then 'access_credential_issued' else 'access_credential_ended' end)::security_event_kind,
      (case when current_user_id() is null then 'scheduled' else 'manual' end)::security_event_method,
      case when new.status = 'active' then new.since else new.until end
    );
  end if;
  return new;
end;
$$;

create or replace function app_private.tg_security_event__user_proxy()
  returns trigger
  language plpgsql security definer
  set search_path to pg_catalog, public, pg_temp
as $$
begin
  if tg_op = 'DELETE' then
    if old.status = 'active' then
      insert into security_event (user_id, person_id, kind, method)
      values (old.user_id, old.person_id, 'person_unlinked', 'manual');
    end if;
    return old;
  end if;

  if tg_op = 'INSERT' and new.status = 'active' then
    insert into security_event (user_id, person_id, kind, method, effective_at)
    values (new.user_id, new.person_id, 'person_linked', 'manual', new.since);
  elsif tg_op = 'UPDATE'
     and new.status is distinct from old.status
     and new.status in ('active', 'expired') then
    insert into security_event (user_id, person_id, kind, method, effective_at)
    values (
      new.user_id,
      new.person_id,
      (case when new.status = 'active' then 'person_linked' else 'person_unlinked' end)::security_event_kind,
      (case when current_user_id() is null then 'scheduled' else 'manual' end)::security_event_method,
      case when new.status = 'active' then new.since else new.until end
    );
  end if;
  return new;
end;
$$;

create or replace function app_private.tg_security_event__password()
  returns trigger
  language plpgsql security definer
  set search_path to pg_catalog, public, pg_temp
as $$
begin
  insert into security_event (user_id, kind, method)
  values (new.id, 'password_changed', 'manual');
  return new;
end;
$$;

create or replace trigger _050_relationship_status
  before insert or update of since, until on user_proxy
  for each row execute function app_private.tg_relationship__status();
create or replace trigger _050_relationship_status
  before insert or update of since, until on tenant_membership
  for each row execute function app_private.tg_relationship__status();
create or replace trigger _050_relationship_status
  before insert or update of since, until on tenant_trainer
  for each row execute function app_private.tg_relationship__status();
create or replace trigger _050_relationship_status
  before insert or update of since, until on tenant_administrator
  for each row execute function app_private.tg_relationship__status();
create or replace trigger _050_relationship_status
  before insert or update of since, until on access_credential
  for each row execute function app_private.tg_relationship__status();

create or replace trigger _900_security_event
  after insert or update of status or delete on tenant_membership
  for each row execute function app_private.tg_security_event__range(
    'membership_granted', 'membership_revoked'
  );
create or replace trigger _900_security_event
  after insert or update of status or delete on tenant_trainer
  for each row execute function app_private.tg_security_event__range(
    'trainer_granted', 'trainer_revoked'
  );
create or replace trigger _900_security_event
  after insert or update of status or delete on tenant_administrator
  for each row execute function app_private.tg_security_event__range(
    'administrator_granted', 'administrator_revoked'
  );
create or replace trigger _900_security_event
  after insert or update of status or delete on access_credential
  for each row execute function app_private.tg_security_event__credential();
create or replace trigger _900_security_event
  after insert or update of status or delete on user_proxy
  for each row execute function app_private.tg_security_event__user_proxy();
create or replace trigger _900_security_event
  after update of u_pass on users
  for each row
  when (old.u_pass is distinct from new.u_pass)
  execute function app_private.tg_security_event__password();
--! EndIncluded functions/security_events.sql
--! Included functions/event_instances_for_range.sql
do $$
begin
  create type event_instance_range_scope as enum ('all', 'top_level', 'mine', 'relevant');
exception when duplicate_object then null;
end
$$;

drop function if exists event_instances_for_range;

create or replace function event_instances_for_range(
  only_type event_type,
  start_range timestamptz,
  end_range timestamptz = null,
  trainer_ids bigint[] = null,
  participant_ids bigint[] = null,
  parent_id bigint = null,
  scope event_instance_range_scope = 'all',
  location_ids bigint[] = null
) returns setof event_instance as $$
  with mine as (
    select instance_id from event_instance_registration
    where person_id = any (current_person_ids()) and registration_status = 'active'
    union all
    select instance_id from event_instance_trainer
    where person_id = any (current_person_ids())
  )
  select i.*
  from event_instance i
  where i.tenant_id = current_tenant_id()
    and (only_type is null or i.type = only_type)
    and case
      when $6 is not null then i.parent_id = $6
        and (scope <> 'mine' or i.id in (select instance_id from mine))
      when scope = 'all' then true
      when scope = 'top_level' then i.parent_id is null
      when scope = 'mine' then i.id in (select instance_id from mine)
      when scope = 'relevant' then i.parent_id is null
        or i.id in (select instance_id from mine)
        or i.parent_id in (select instance_id from mine)
    end
    and i.since < coalesce(end_range, 'infinity'::timestamptz)
    and i.until > start_range
    and (trainer_ids is null
      or exists (select 1 from event_instance_trainer where instance_id = i.id and person_id = any (trainer_ids)))
    and (participant_ids is null
      or exists (select 1 from event_instance_registration where instance_id = i.id and person_id = any (participant_ids) and registration_status = 'active'))
    and (location_ids is null or i.location_id = any (location_ids))
  order by i.since
  ;
$$ stable language sql;

comment on function event_instances_for_range is '@simpleCollections only';
grant all on function event_instances_for_range to anonymous;
--! EndIncluded functions/event_instances_for_range.sql
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
    and location.is_public

  union

  select cover_image_id
  from tenant_location
  where tenant_id = (select current_tenant_id())
    and is_public
    and cover_image_id is not null;
$$;

grant execute on function app_private.visible_file_ids() to anonymous;
--! EndIncluded functions/visible_file_ids.sql
--! Included functions/upsert_location.sql
do $$
begin
  if not exists (
    select 1
    from pg_type type
    join pg_namespace namespace on namespace.oid = type.typnamespace
    where namespace.nspname = 'public' and type.typname = 'location_details_input'
  ) then
    create type location_details_input as (
      id bigint,
      name text,
      description text,
      address address_domain,
      is_public boolean
    );
  end if;
end;
$$;

do $$
begin
  if to_regtype('location_image_input') is not null then
    execute 'drop function if exists upsert_location(location_details_input, location_image_input[])';
    execute 'drop type location_image_input';
  end if;
end;
$$;

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
      description,
      address,
      is_public,
      cover_image_id
    )
    values (
      details.name,
      coalesce(details.description, ''),
      details.address,
      coalesce(details.is_public, true),
      cover_image_id
    )
    returning * into result;
  else
    update tenant_location
    set name = details.name,
        description = coalesce(details.description, ''),
        address = details.address,
        is_public = coalesce(details.is_public, true),
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
