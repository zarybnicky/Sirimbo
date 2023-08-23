--! Previous: sha1:de34ebe91645643c1ace60ddd493d07a13eef499
--! Hash: sha1:1bfce4db238e7c9cddd66cf8c5e2ef0486be3dd1

--! split: 1-current.sql
CREATE or replace FUNCTION public.filtered_people(in_tenants bigint[], in_cohort bigint, is_trainer boolean, is_admin boolean) RETURNS SETOF public.person
    LANGUAGE sql STABLE
    AS $$
 select person.* from person
  where
    exists (select 1 from tenant_membership where tenant_id = any (in_tenants) and person_id=person.id and active=true)
  and
    case when in_cohort is null then true
    else exists (select 1 from cohort_membership where cohort_id=in_cohort and person_id=person.id and active=true) end
  and
    case when is_trainer is null then true
    else is_trainer = exists (select 1 from tenant_trainer where tenant_id = any (in_tenants) and person_id=person.id) end
  and
    case when is_admin is null then true
    else is_admin = exists (select 1 from tenant_administrator where tenant_id = any (in_tenants) and person_id=person.id) end
$$;

CREATE or replace FUNCTION public.person_couples(p person) RETURNS SETOF public.couple LANGUAGE sql STABLE AS $$
  select couple.* from couple where (man_id = p.id or woman_id = p.id);
$$;
CREATE or replace FUNCTION public.person_active_couples(p person) RETURNS SETOF public.couple LANGUAGE sql STABLE AS $$
  select couple.* from couple where (man_id = p.id or woman_id = p.id) and active = true;
$$;
GRANT ALL ON FUNCTION public.person_active_couples(person) TO anonymous;
comment on function person_active_couples is E'@simpleCollections only';

CREATE or replace FUNCTION public.couple_attendances(p couple) RETURNS SETOF public.event_attendance LANGUAGE sql STABLE AS $$
  select event_attendance.* from event_attendance
  where person_id = p.man_id or person_id = p.woman_id;
$$;
GRANT ALL ON FUNCTION public.couple_attendances(couple) TO anonymous;
comment on function couple_attendances is E'@simpleCollections only
@filterable
@sortable';

grant all on function current_person_ids to anonymous;

drop table if exists person_invitation;
create table person_invitation (
  id bigint PRIMARY KEY GENERATED ALWAYS AS IDENTITY,
  access_token uuid default gen_random_uuid() not null unique,
  person_id bigint references person (id),
  tenant_id bigint DEFAULT public.current_tenant_id() NOT NULL,
  used_at timestamptz DEFAULT null,
  created_at timestamptz DEFAULT now() NOT NULL,
  updated_at timestamptz DEFAULT now() NOT NULL
);
comment on table person_invitation is E'@omit';
create index on person_invitation (person_id);
alter table person_invitation enable row level security;
grant all on person_invitation to anonymous;
CREATE POLICY admin_create on public.person_invitation using (exists (select 1 from tenant_administrator where tenant_administrator.person_id = any (current_person_ids()) and tenant_administrator.tenant_id = current_tenant_id()));
CREATE POLICY my_tenant ON public.person_invitation AS RESTRICTIVE USING (tenant_id = current_tenant_id());

CREATE or replace FUNCTION app_private.tg_person_invitation__send() RETURNS trigger LANGUAGE plpgsql SECURITY DEFINER AS $$
begin
  perform graphile_worker.add_job('send_invitation', json_build_object('id', NEW.id));
  return NEW;
end;
$$;
select verify_function('app_private.tg_person_invitation__send', 'person_invitation');
drop trigger if exists _500_send on public.person_invitation;
CREATE TRIGGER _500_send
  AFTER INSERT ON public.person_invitation
  FOR EACH ROW EXECUTE FUNCTION app_private.tg_person_invitation__send();


drop table if exists otp_token;
create table otp_token (
  id bigint PRIMARY KEY GENERATED ALWAYS AS IDENTITY,
  access_token uuid default gen_random_uuid() not null unique,
  user_id bigint references users (u_id),
  tenant_id bigint DEFAULT public.current_tenant_id() NOT NULL,
  expires_at timestamptz DEFAULT now() + interval '24 hour' NOT NULL,
  used_at timestamptz DEFAULT null,
  created_at timestamptz DEFAULT now() NOT NULL,
  updated_at timestamptz DEFAULT now() NOT NULL
);
comment on table otp_token is E'@omit';
create index on otp_token (user_id);
alter table otp_token enable row level security;
grant all on person_invitation to anonymous;

alter table person alter column birth_date drop not null;

drop function if exists create_person;
create or replace function public.create_person(inout p person, primary_email text, primary_phone text, is_member boolean, is_trainer boolean, is_admin boolean, send_invitation boolean, join_date timestamptz) language plpgsql as $$
begin
  insert into person overriding user value select p.* returning * into p;
  if is_member = true then
    insert into tenant_membership (person_id, tenant_id, since, active) values (p.id, current_tenant_id(), join_date, true);
  end if;
  if is_trainer = true then
    insert into tenant_trainer (person_id, tenant_id, since, active) values (p.id, current_tenant_id(), join_date, true);
  end if;
  if is_admin = true then
    insert into tenant_administrator (person_id, tenant_id, since, active) values (p.id, current_tenant_id(), join_date, true);
  end if;
  if primary_phone is not null then
    insert into person_phone (person_id, phone, is_primary) values (p.id, primary_phone, true);
  end if;
  if primary_email is not null then
    insert into person_email (person_id, email, is_primary) values (p.id, primary_email, true);
  end if;

  if send_invitation = true then
    insert into person_invitation (person_id, tenant_id) values (p.id, current_tenant_id());
  end if;
end
$$;
select verify_function('create_person');
grant all on function create_person to anonymous;
