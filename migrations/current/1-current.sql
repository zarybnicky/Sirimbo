--!include functions/register_without_invitation.sql

create or replace function postgraphile_watch.notify_watchers_ddl() returns event_trigger as $$
declare
  ddl_commands json;
begin
  select json_agg(
    json_build_object('schema', schema_name, 'command', command_tag)
  ) into ddl_commands
  from pg_event_trigger_ddl_commands();

  if json_array_length(ddl_commands) > 0 then
    perform pg_notify(
      'postgraphile_watch',
      json_build_object('type', 'ddl', 'payload', ddl_commands)::text
    );
  end if;
end;
$$ language plpgsql;

create or replace function postgraphile_watch.notify_watchers_drop() returns event_trigger as $$
declare
  objects json;
begin
  select json_agg(distinct schema_name) into objects
  from pg_event_trigger_dropped_objects()
  where schema_name <> 'pg_temp';

  if json_array_length(objects) > 0 then
    perform pg_notify(
      'postgraphile_watch',
      json_build_object('type', 'drop', 'payload', objects)::text
    );
  end if;
end;
$$ language plpgsql;

create or replace view announcement as
  select
    up_id as id,
    tenant_id,
    up_kdo as author_id,
    up_nadpis as title,
    up_text as body,
    up_lock as is_locked,
    created_at,
    updated_at,
    scheduled_since,
    scheduled_until,
    is_visible,
    sticky as is_sticky,
    up_timestamp as last_activity_at,
    up_timestamp_add as published_at
  from upozorneni;

grant all on announcement to anonymous;

comment on view announcement is E'@primaryKey id
@foreignKey (tenant_id) references tenant (id)
@foreignKey (author_id) references users (id)
@simpleCollections only';

comment on column announcement.id is '@hasDefault';
comment on column announcement.title is '@notNull';
comment on column announcement.body is '@notNull';
comment on column announcement.is_locked is E'@notNull
@hasDefault';
comment on column announcement.tenant_id is '@hasDefault';
comment on column announcement.is_sticky is E'@notNull
@hasDefault';
comment on column announcement.created_at is E'@notNull
@hasDefault';
comment on column announcement.last_activity_at is E'@notNull
@hasDefault';
comment on column announcement.published_at is E'@notNull
@hasDefault';

comment on table public.upozorneni_skupiny is E'@omit create,update,delete
@foreignKey (ups_id_rodic) references announcement (id)
@foreignKey (ups_id_skupina) references cohort (id)';
comment on constraint upozorneni_skupiny_ups_id_rodic_fkey on upozorneni_skupiny is '@fieldName upozorneni';

