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

--!include functions/security_events.sql
--!include functions/event_instances_for_range.sql
--!include functions/visible_file_ids.sql
--!include functions/upsert_location.sql
