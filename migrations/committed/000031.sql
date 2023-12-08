--! Previous: sha1:7bf5c19ffde28889b1145f1dad3c988710d46104
--! Hash: sha1:6ba93ea08f57aee2d0571a96d3b24ae6d7e6bf99

--! split: 1-current.sql
CREATE or replace FUNCTION register_without_invitation(email text, login text, passwd text, OUT usr users, OUT jwt jwt_token) RETURNS record LANGUAGE plpgsql STRICT SECURITY DEFINER AS $$
declare
  v_salt text;
begin
  select encode(digest('######TK.-.OLYMP######', 'md5'), 'hex') into v_salt;
  insert into users (u_login, u_email, u_pass) values (trim(login), email, encode(digest(v_salt || passwd || v_salt, 'sha1'), 'hex')) returning * into usr;
  jwt := app_private.create_jwt_token(usr);
  perform set_config('jwt.claims.user_id', jwt.user_id::text, true);
  perform set_config('jwt.claims.my_person_ids', jwt.my_person_ids::text, true);
  perform set_config('jwt.claims.my_tenant_ids', jwt.my_tenant_ids::text, true);
  perform set_config('jwt.claims.my_cohort_ids', jwt.my_cohort_ids::text, true);
  perform set_config('jwt.claims.my_couple_ids', jwt.my_couple_ids::text, true);
end
$$;

GRANT ALL ON FUNCTION register_without_invitation TO anonymous;

CREATE or replace FUNCTION app_private.create_jwt_token(u public.users) RETURNS public.jwt_token
    LANGUAGE sql STABLE
    AS $$
  with details as (
    SELECT
      u_id,
      user_proxy.person_id as person_id,
      tenant_memberships || tenant_trainers || tenant_administrators as my_tenant_ids,
      cohort_memberships as my_cohort_ids,
      couple_ids as my_couple_ids,
      current_tenant_id() = ANY (tenant_memberships || tenant_trainers || tenant_administrators) as is_member,
      current_tenant_id() = ANY (tenant_trainers) as is_trainer,
      current_tenant_id() = ANY (tenant_administrators) as is_admin
    from users
    left join user_proxy on user_id=users.u_id
    left join auth_details on user_proxy.person_id=auth_details.person_id
    where users.u_id=u.u_id
  ) select
    extract(epoch from now() + interval '7 days')::integer,
    u.u_id,
    current_tenant_id(),
    u.u_login,
    u.u_email,
    array_to_json(array_agg(person_id)) as my_person_ids,
    array_to_json(array_accum(my_tenant_ids)) as my_tenant_ids,
    array_to_json(array_accum(my_cohort_ids)) as my_cohort_ids,
    array_to_json(array_accum(my_couple_ids)) as my_couple_ids,
    bool_or(is_member) as is_member,
    bool_or(is_trainer) as is_trainer,
    bool_or(is_admin) as is_admin
  from details group by u_id;
$$;

comment on table membership_application is '@simpleCollections only';
select app_private.drop_policies('public.membership_application');
alter table membership_application enable row level security;
grant select, delete on membership_application to anonymous;
grant insert (
  first_name, middle_name, last_name, gender, birth_date, nationality, tax_identification_number,
  national_id_number, csts_id, wdsf_id, prefix_title, suffix_title, bio, email, phone
) on membership_application to anonymous;
grant update (
  first_name, middle_name, last_name, gender, birth_date, nationality, tax_identification_number,
  national_id_number, csts_id, wdsf_id, prefix_title, suffix_title, bio, email, phone
) on membership_application to anonymous;
grant all on membership_application to administrator;
create policy manage_admin on membership_application to administrator using (true);
create policy manage_my on membership_application to anonymous using (created_by = current_user_id());
CREATE POLICY my_tenant ON membership_application AS RESTRICTIVE USING (tenant_id = current_tenant_id());
alter table membership_application alter column status set default 'sent';

CREATE or replace FUNCTION confirm_membership_application(application_id bigint) RETURNS person LANGUAGE sql AS $$
  with t_person as (
    insert into person (
      first_name, middle_name, last_name, gender, birth_date, nationality, tax_identification_number,
      national_id_number, csts_id, wdsf_id, prefix_title, suffix_title, bio, email, phone
    ) select
      first_name, middle_name, last_name, gender, birth_date, nationality, tax_identification_number,
      national_id_number, csts_id, wdsf_id, prefix_title, suffix_title, bio, email, phone
    from membership_application where id = application_id and status='sent'
    returning *
  ), appl as (
     update membership_application set status='approved' where id=application_id
  ), member as (
    insert into tenant_membership (tenant_id, person_id)
    values (current_tenant_id(), (select id from t_person))
    returning *
  ), proxy as (
    insert into user_proxy (person_id, user_id)
    values ((select id from t_person), (select created_by from membership_application where id = application_id))
  ) select * from t_person;
$$;
grant all on function confirm_membership_application to administrator;

create or replace function reject_membership_application(application_id bigint) returns membership_application language sql as $$
  update membership_application set status='rejected' where id=application_id returning *;
$$;
grant all on function confirm_membership_application to administrator;

alter table transaction add column if not exists effective_date timestamptz null;
update transaction set effective_date=created_at where effective_date is null;
CREATE or replace FUNCTION app_private.tg_transaction__effective_date() RETURNS trigger LANGUAGE plpgsql AS $$
begin
  if NEW.effective_date is null then
    NEW.effective_date = NEW.created_at;
  end if;
  return NEW;
end;
$$;
CREATE or replace TRIGGER _300_effective_date BEFORE INSERT OR UPDATE ON public.transaction FOR EACH ROW EXECUTE PROCEDURE app_private.tg_transaction__effective_date();

drop function if exists create_credit_transaction;
create or replace function create_credit_transaction(v_account_id bigint, v_description text, v_amount numeric(19, 4), v_date timestamptz default now()) returns transaction language sql as $$
  with txn as (
    insert into transaction (source, description, effective_date) values ('manual-credit', v_description, v_date) returning *
  ), posting as (
    insert into posting (transaction_id, account_id, amount) values ((select id from txn), v_account_id, v_amount) returning *
  )
  select * from txn
$$;
grant all on function create_credit_transaction to anonymous;

do $$
begin
  if not exists (select 1 from information_schema.constraint_column_usage where constraint_name = 'account_tenant_id_person_id_currency_idx') then
    drop index if exists account_tenant_id_person_id_currency_idx;
  end if;
end
$$;
alter table account
  drop constraint if exists account_tenant_id_person_id_currency_idx,
 add constraint account_tenant_id_person_id_currency_idx unique nulls not distinct (tenant_id, person_id, currency);

CREATE or replace FUNCTION public.person_account(p_id bigint, c text, OUT acc public.account) RETURNS public.account
    LANGUAGE sql SECURITY DEFINER
    AS $$
  with inserted as (
    insert into account (tenant_id, person_id, currency)
    values (current_tenant_id(), p_id, coalesce(c, 'CZK'))
    on conflict on constraint account_tenant_id_person_id_currency_idx do nothing
    returning *
  )
  select * from inserted union select * from account where person_id=p_id and currency=c and tenant_id=current_tenant_id();
$$;

CREATE or replace FUNCTION public.tenant_account(c text, OUT acc public.account) RETURNS public.account
    LANGUAGE sql SECURITY DEFINER
    AS $$
  with inserted as (
    insert into account (tenant_id, person_id, currency)
    values (current_tenant_id(), null, coalesce(c, 'CZK'))
    on conflict on constraint account_tenant_id_person_id_currency_idx do nothing
    returning *
  )
  select * from inserted union select * from account where person_id is null and currency=c and tenant_id=current_tenant_id();
$$;
