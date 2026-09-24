drop function if exists access_credential_is_allowed;
drop function if exists access_credential_last_used;
drop table if exists access_credential;

do $$
begin
  if to_regtype('public.access_credential_kind') is null then
    create type access_credential_kind as enum ('MIFARE');
  end if;
end;
$$;

create table if not exists access_credential (
  id bigint generated always as identity primary key,
  tenant_id bigint not null default current_tenant_id() references tenant(id),
  person_id bigint not null references person(id),
  kind access_credential_kind not null default 'MIFARE',
  label text not null check (label <> '' and label = btrim(label)),
  code text not null check (code <> '' and code = btrim(code)),
  since timestamptz not null default now(),
  until timestamptz,
  valid_range tstzrange generated always as (tstzrange(since, until, '[)')) stored,
  created_at timestamptz not null default now(),
  updated_at timestamptz not null default now(),
  created_by bigint default current_user_id() references users(id) on delete set null,
  constraint access_credential_until_gt_since check (until > since),
  constraint access_credential_no_overlap exclude using gist (
    tenant_id with =,
    kind with =,
    code with =,
    valid_range with &&
  )
);

comment on table access_credential is '@omit delete
@simpleCollections only';
comment on column access_credential.valid_range is '@omit';

create index if not exists access_credential_person_idx on access_credential (tenant_id, person_id);

grant all on access_credential to anonymous;
alter table access_credential enable row level security;

select app_private.drop_policies('public.access_credential');
create policy current_tenant on access_credential as restrictive
  using (tenant_id = current_tenant_id());
create policy admin_view on access_credential for select to administrator using (true);
create policy admin_insert on access_credential for insert to administrator with check (
  exists (select 1 from tenant_membership r where r.tenant_id = access_credential.tenant_id and r.person_id = access_credential.person_id)
  or exists (select 1 from tenant_trainer r where r.tenant_id = access_credential.tenant_id and r.person_id = access_credential.person_id)
  or exists (select 1 from tenant_administrator r where r.tenant_id = access_credential.tenant_id and r.person_id = access_credential.person_id)
);
create policy admin_update on access_credential for update to administrator with check (
  exists (select 1 from tenant_membership r where r.tenant_id = access_credential.tenant_id and r.person_id = access_credential.person_id)
  or exists (select 1 from tenant_trainer r where r.tenant_id = access_credential.tenant_id and r.person_id = access_credential.person_id)
  or exists (select 1 from tenant_administrator r where r.tenant_id = access_credential.tenant_id and r.person_id = access_credential.person_id)
);

create or replace trigger _100_timestamps before insert or update on access_credential
  for each row execute function app_private.tg__timestamps();

create or replace function access_credential_is_allowed(c access_credential) returns boolean language sql stable
as $$
  select c.valid_range @> now() and (
    exists (select 1 from current_tenant_membership r where r.person_id = c.person_id)
    or exists (select 1 from current_tenant_trainer r where r.person_id = c.person_id)
    or exists (select 1 from current_tenant_administrator r where r.person_id = c.person_id)
  );
$$;
grant all on function access_credential_is_allowed(access_credential) to anonymous;

create table if not exists access_event (
  id bigint generated always as identity primary key,
  tenant_id bigint not null default current_tenant_id() references tenant(id),
  external_id text not null check (external_id <> '' and external_id = btrim(external_id)),
  device text not null check (device <> '' and device = btrim(device)),
  kind access_credential_kind not null,
  code text not null check (code <> '' and code = btrim(code)),
  person_id bigint references person(id),
  occurred_at timestamptz not null,
  received_at timestamptz not null default now(),
  allowed boolean not null,
  reason text not null default '',
  unique (tenant_id, external_id)
);

comment on table access_event is '@omit create,update,delete
@simpleCollections only';

create index if not exists access_event_person_idx
  on access_event (tenant_id, person_id, occurred_at desc);
create index if not exists access_event_credential_idx
  on access_event (tenant_id, kind, code, occurred_at desc);

grant all on access_event to anonymous;
alter table access_event enable row level security;

select app_private.drop_policies('public.access_event');
create policy current_tenant on access_event as restrictive
  using (tenant_id = current_tenant_id());
create policy admin_view on access_event for select to administrator using (true);
create policy my_view on access_event for select using (person_id = any(current_person_ids()));

create or replace function access_credential_last_used(c access_credential) returns timestamptz
language sql stable
as $$
  select max(e.occurred_at)
  from access_event e
  where e.tenant_id = c.tenant_id
    and e.kind = c.kind
    and e.code = c.code
    and e.allowed
    and c.valid_range @> e.occurred_at;
$$;
grant all on function access_credential_last_used(access_credential) to anonymous;

--!include functions/system_admin_tenants.sql
--!include functions/system_admin_update_tenant.sql
--!include functions/event_instance_approx_price.sql
