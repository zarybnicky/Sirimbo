--! Previous: sha1:c5e51c419f94c1e8c8130b3c78b0c4619688c75f
--! Hash: sha1:de34ebe91645643c1ace60ddd493d07a13eef499

--! split: 1-current.sql
-- Write your migration here

comment on table person is E'@omit create,delete';
comment on table platby_item is E'@omit create,update,delete';
comment on table platby_category is E'@omit create,update,delete';
comment on table platby_category_group is E'@omit';
comment on table platby_group is E'@omit';
comment on table platby_group_skupina is E'@omit';
comment on table platby_raw is E'@omit';
comment on table tenant_attachment is E'@omit create,update,delete';
comment on table room_attachment is E'@omit create,update,delete';
comment on table location_attachment is E'@omit create,update,delete';
comment on table upozorneni_skupiny is E'@omit create,update,delete';
comment on table dokumenty is E'@simpleCollections only';
comment on table attachment is E'@omit update';

COMMENT ON TABLE public.tenant_location IS '@omit create,update,delete
@simpleCollections only';

select app_private.drop_policies('public.person');
create policy admin_all on person to administrator using (true);
create policy admin_myself on person for update using (id in (select my_person_ids()));
create policy view_same_tenant on person for select using (exists (select 1 from tenant_membership where active=true and person_id = id and tenant_id in (select my_tenant_ids())));
create policy view_tenant_admin on person for select using (exists (select 1 from tenant_administrator where active=true and person_id = id));
create policy view_tenant_trainer on person for select using (exists (select 1 from tenant_trainer where active=true and person_id = id));

CREATE or replace FUNCTION public.login(login character varying, passwd character varying, OUT sess public.session, OUT usr public.users) RETURNS record
    LANGUAGE plpgsql STRICT SECURITY DEFINER
    AS $$
declare
  v_salt varchar;
begin
  if login like '%@%' then
    select users.* into usr from users where lower(u_email) = lower(login) limit 1;
  else
    select users.* into usr from users where lower(u_login) = lower(login) limit 1;
  end if;

  if usr is null then
    raise exception 'ACCOUNT_NOT_FOUND' using errcode = '28000';
  end if;

  select encode(digest('######TK.-.OLYMP######', 'md5'), 'hex') into v_salt;
  if usr.u_pass != encode(digest(v_salt || passwd || v_salt, 'sha1'), 'hex') then
    raise exception 'INVALID_PASSWORD' using errcode = '28P01';
  end if;

  perform set_config('jwt.claims.user_id', usr.u_id::text, true);
  insert into session (ss_id, ss_user, ss_lifetime) values (gen_random_uuid(), usr.u_id, 86400)
  returning * into sess;
end;
$$;

GRANT ALL ON FUNCTION person_is_trainer TO anonymous;

do $$ begin
  if not exists (select 1 from information_schema.columns where table_name = 'tenant_location' and column_name = 'id') then
    ALTER TABLE tenant_location DROP CONSTRAINT tenant_location_pkey;
    ALTER TABLE tenant_location ADD COLUMN id bigint GENERATED ALWAYS AS IDENTITY PRIMARY KEY;
  end if;
end $$;

create index if not exists tenant_location_tenant_id_idx on tenant_location (tenant_id);

drop type if exists address_type cascade;
do $$ begin
  if not exists (select 1 from pg_type where typname = 'address_type' and typcategory = 'C') then
    create type address_type as (
      street text,
      conscription_number text,
      orientation_number text,
      district text,
      city text,
      region text,
      postal_code text
    );
    create domain address_domain as address_type check (
      value is null or (
        (value).street is not null and
        (value).conscription_number is not null and
        (value).orientation_number is not null and
        (value).city is not null and
        (value).region is not null and
        (value).postal_code is not null
      )
    );
  end if;
end $$;

drop type if exists price_type cascade;
do $$ begin
  if not exists (select 1 from pg_type where typname = 'price' and typtype = 'd') then
    create type price_type as (
      amount numeric(19, 4),
      currency text
    );
    create domain price as price_type check (
      value is null or (
        (value).currency is not null and length((value).currency) = 3 and
        (value).amount is not null
      )
    );
  end if;
end $$;

COMMENT ON CONSTRAINT "person_address_address_id_fkey" ON "public"."person_address" IS E'@fieldName address_legacy';
alter table person_address add column if not exists address address_domain;
update person_address
set address = (select (street, conscription_number, orientation_number, district, city, '', postal_code)::address_domain from address where address_id=id);
comment on column person_address.address_id is '@omit';
comment on table address is '@omit';

alter table location add column if not exists address address_domain null;
alter table tenant add column if not exists cz_ico text not null default '';
alter table tenant add column if not exists cz_dic text not null default '';
alter table tenant add column if not exists address address_domain null;
alter table tenant_administrator add column if not exists is_visible boolean not null default true;
alter table tenant_administrator add column if not exists description text not null default '';
alter table tenant_trainer add column if not exists is_visible boolean default true;
alter table tenant_trainer add column if not exists description text not null default '';
alter table tenant_trainer add column if not exists default_price price null default null;
alter table event_trainer add column if not exists lesson_price price null default null;
alter table event add column if not exists registration_price price null default null;

comment on column users.u_pass is E'@omit';
comment on column users.u_pohlavi is E'@omit';
comment on column users.u_narozeni is E'@omit';
comment on column users.u_telefon is E'@omit';
comment on column users.u_rodne_cislo is E'@omit';
comment on column users.u_poznamky is E'@omit';
comment on column users.u_level is E'@omit';
comment on column users.u_group is E'@omit';
comment on column users.u_skupina is E'@omit';
comment on column users.u_dancer is E'@omit';
comment on column users.u_ban is E'@omit';
comment on column users.u_lock is E'@omit';
comment on column users.u_confirmed is E'@omit';
comment on column users.u_system is E'@omit';
comment on column users.u_teacher is E'@omit';
comment on column users.u_member_since is E'@omit';
comment on column users.u_member_until is E'@omit';
comment on column users.u_gdpr_signed_at is E'@omit';
