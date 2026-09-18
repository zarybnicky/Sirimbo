drop function if exists access_credential_is_allowed;
drop table if exists access_credential;

create table if not exists access_credential (
  id bigint generated always as identity primary key,
  tenant_id bigint not null default current_tenant_id() references tenant(id),
  person_id bigint not null references person(id),
  uid text not null check (uid <> '' and uid = btrim(uid)),
  label text not null default '',
  since timestamptz not null default now(),
  until timestamptz,
  valid_range tstzrange generated always as (tstzrange(since, until, '[)')) stored,
  created_at timestamptz not null default now(),
  updated_at timestamptz not null default now(),
  created_by bigint default current_user_id() references users(id) on delete set null,
  constraint access_credential_until_gt_since check (until > since)
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
