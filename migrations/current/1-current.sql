
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
