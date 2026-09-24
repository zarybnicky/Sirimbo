--! Previous: sha1:ab3ea254ab9f11ea7a156bff384659ab27db556f
--! Hash: sha1:52e81a7f345f8ef0bd36ae1ee5068c248508d3af

--! split: 1-current.sql
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

--! Included functions/system_admin_tenants.sql
drop function if exists system_admin_tenants;
create function system_admin_tenants()
returns table (
  id bigint,
  name text,
  description text,
  bank_account text,
  origins text[],
  cz_ico text,
  cz_dic text,
  address address_domain,
  settings text,
  membership_count bigint,
  trainer_count bigint,
  administrator_count bigint,
  session_count_last_30_days bigint,
  session_count_per_trainer_last_30_days double precision
)
  language plpgsql
  stable
  security definer
  set search_path = pg_catalog, public, pg_temp
as $$
begin
  if not app_private.is_system_admin(current_user_id()) then
    raise exception 'permission denied for system admin tenant overview'
      using errcode = '42501';
  end if;

  return query
  select
    t.id,
    t.name,
    t.description,
    t.bank_account,
    t.origins,
    t.cz_ico,
    t.cz_dic,
    t.address,
    coalesce(ts.settings::text, '{}'),
    membership_counts.membership_count,
    staffing.trainer_count,
    administrators.administrator_count,
    load.session_count_last_30_days,
    load.session_count_per_trainer_last_30_days
  from tenant t
  left join tenant_settings ts on ts.tenant_id = t.id
  cross join lateral (
    select count(*) as membership_count from tenant_membership tm where tm.tenant_id = t.id and tm.status = 'active'
  ) as membership_counts
  cross join lateral (
    select count(*) as trainer_count from tenant_trainer tt where tt.tenant_id = t.id and tt.status = 'active'
  ) as staffing
  cross join lateral (
    select count(*) as administrator_count from tenant_administrator ta where ta.tenant_id = t.id and ta.status = 'active'
  ) as administrators
  cross join lateral (
    select
      count(*) as session_count_last_30_days,
      case
        when coalesce(staffing.trainer_count, 0) > 0 then count(*)::double precision / staffing.trainer_count::double precision
        else 0::double precision
      end as session_count_per_trainer_last_30_days
    from event_instance ei
    where ei.tenant_id = t.id
      and coalesce(ei.is_cancelled, false) = false
      and ei.since >= now() - interval '30 days'
  ) as load
  order by t.name;
end;
$$;

grant execute on function system_admin_tenants to anonymous;
--! EndIncluded functions/system_admin_tenants.sql
--! Included functions/system_admin_update_tenant.sql
drop function if exists system_admin_update_tenant;
create function system_admin_update_tenant(
  tenant_id bigint,
  name text default null,
  description text default null,
  bank_account text default null,
  origins text[] default null,
  address address_domain default null,
  cz_ico text default null,
  cz_dic text default null,
  settings jsonb default null
)
returns tenant
language plpgsql
volatile
security definer
set search_path = public, pg_temp
as $$
declare
  v_tenant tenant;
begin
  if not app_private.is_system_admin(current_user_id()) then
    raise exception 'permission denied for system admin tenant update'
      using errcode = '42501';
  end if;

  update tenant t
  set
    name = coalesce(system_admin_update_tenant.name, t.name),
    description = coalesce(system_admin_update_tenant.description, t.description),
    bank_account = coalesce(system_admin_update_tenant.bank_account, t.bank_account),
    origins = coalesce(system_admin_update_tenant.origins, t.origins),
    address = coalesce(system_admin_update_tenant.address, t.address),
    cz_ico = coalesce(system_admin_update_tenant.cz_ico, t.cz_ico),
    cz_dic = coalesce(system_admin_update_tenant.cz_dic, t.cz_dic)
  where t.id = tenant_id
  returning t.* into v_tenant;

  if not found then
    raise exception 'tenant % not found', tenant_id using errcode = 'P0002';
  end if;

  if settings is not null then
    update tenant_settings ts
    set settings = system_admin_update_tenant.settings
    where ts.tenant_id = system_admin_update_tenant.tenant_id;
  end if;

  return v_tenant;
end;
$$;

grant execute on function system_admin_update_tenant to anonymous;
--! EndIncluded functions/system_admin_update_tenant.sql
--! Included functions/event_instance_approx_price.sql
create or replace function event_instance_approx_price(v_instance event_instance)
  returns table (amount numeric(19,4), currency text)
  language sql stable
as $$
  with stats as materialized (
    select
      count(*) as num_participants,
      extract(epoch from (v_instance.until - v_instance.since)) / 60.0 as duration
    from event_instance_registration registration
    where
      v_instance.type = 'lesson'
      and registration.instance_id = v_instance.id
      and registration.person_id is not null
      and registration.registration_status = 'active'
  )
  select
    sum(tt.member_price_45min_amount * s.duration / 45 / s.num_participants) as amount,
    tt.currency as currency
  from stats s
  join lateral event_instance_trainers(v_instance) tt on true
  where
    s.num_participants > 0
    and s.duration > 0
    and tt.member_price_45min_amount is not null
    and tt.currency is not null
  group by tt.currency;
$$;

grant all on function event_instance_approx_price to anonymous;
comment on function event_instance_approx_price is '@simpleCollections only';
--! EndIncluded functions/event_instance_approx_price.sql
--! Included functions/tg_tenant_membership__on_status.sql
create or replace function app_private.tg_tenant_membership__on_status()
  returns trigger
  language plpgsql
as $$
begin
  if new.status = 'expired' and not exists (
    select from tenant_membership
    where id <> new.id
      and tenant_id = new.tenant_id
      and person_id = new.person_id
      and active_range @> new.until
  ) then
    update cohort_membership
    set status = 'expired', until = new.until
    where tenant_id = new.tenant_id
      and person_id = new.person_id
      and since < new.until
      and (until is null or until > new.until);
  end if;
  return new;
end;
$$;

select verify_function('app_private.tg_tenant_membership__on_status', 'tenant_membership');

create or replace trigger _500_on_status
  after update on tenant_membership
  for each row
  when (old.status is distinct from new.status)
  execute function app_private.tg_tenant_membership__on_status();
--! EndIncluded functions/tg_tenant_membership__on_status.sql
