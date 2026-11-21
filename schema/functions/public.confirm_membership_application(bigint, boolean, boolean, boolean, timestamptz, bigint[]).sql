CREATE FUNCTION public.confirm_membership_application(application_id bigint, is_member boolean DEFAULT true, is_trainer boolean DEFAULT false, is_admin boolean DEFAULT false, join_date timestamp with time zone DEFAULT now(), cohort_ids bigint[] DEFAULT ARRAY[]::bigint[]) RETURNS public.person
    LANGUAGE plpgsql SECURITY DEFINER
    SET search_path TO 'pg_catalog', 'public', 'pg_temp'
    AS $$
declare
  result person;
begin
  if not pg_has_role(current_setting('role'), 'administrator', 'member') then
    raise insufficient_privilege using message = 'Cannot confirm this application';
  end if;
  if (is_member or is_trainer or is_admin) is not true then
    raise insufficient_privilege using message = 'A new person must have a club relationship';
  end if;
  if exists (
    select from unnest(cohort_ids) selected(id)
    where not exists (select from cohort where cohort.id = selected.id and tenant_id = current_tenant_id())
  ) then
    raise insufficient_privilege using message = 'Cannot add to another club''s cohort';
  end if;

  with application as materialized (
    select *
    from membership_application
    where id = application_id and status = 'sent' and tenant_id = current_tenant_id()
    for update
  ), t_person as (
    insert into person (
      first_name, last_name, gender, birth_date, nationality, tax_identification_number,
      national_id_number, csts_id, wdsf_id, prefix_title, suffix_title, bio, email, phone,
      note
    )
    select
      first_name, last_name, gender, birth_date, nationality, tax_identification_number,
      national_id_number, csts_id, wdsf_id,
      prefix_title, suffix_title, bio,
      email, phone, note
    from application
    returning *
  ), appl as (
    update membership_application
    set status = 'approved'
    where id = (select id from application)
  ), member as (
    insert into tenant_membership (tenant_id, person_id, since)
    select current_tenant_id(), id, join_date from t_person where is_member
  ), trainer as (
    insert into tenant_trainer (tenant_id, person_id, since)
    select current_tenant_id(), id, join_date from t_person where is_trainer
  ), administrator as (
    insert into tenant_administrator (tenant_id, person_id, since)
    select current_tenant_id(), id, join_date from t_person where is_admin
  ), cohorts as (
    insert into cohort_membership (cohort_id, person_id, since)
    select cohort_id, t_person.id, join_date
    from t_person
    cross join unnest(cohort_ids) selected(cohort_id)
  ), proxy as (
    insert into user_proxy (person_id, user_id)
    select t_person.id, application.created_by
    from t_person cross join application
  )
  select * into result from t_person;
  return result;
end
$$;

GRANT ALL ON FUNCTION public.confirm_membership_application(application_id bigint, is_member boolean, is_trainer boolean, is_admin boolean, join_date timestamp with time zone, cohort_ids bigint[]) TO administrator;
