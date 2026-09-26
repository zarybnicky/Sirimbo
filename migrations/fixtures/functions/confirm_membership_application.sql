drop function if exists confirm_membership_application(bigint);

create or replace function confirm_membership_application(
  application_id bigint,
  is_member boolean default true,
  is_trainer boolean default false,
  is_admin boolean default false,
  join_date timestamptz default now(),
  cohort_ids bigint[] default array[]::bigint[]
)
  returns person
  language sql
as $$
  with application as materialized (
    select *
    from membership_application
    where id = application_id and status = 'sent'
    for update
  ), t_person as (
    insert into person (
      first_name, last_name, gender, birth_date, nationality, tax_identification_number,
      national_id_number, csts_id, wdsf_id, prefix_title, suffix_title, bio, email, phone,
      note
    )
    select
      first_name, last_name, gender, birth_date, nationality, tax_identification_number,
      national_id_number, csts_id, wdsf_id, prefix_title, suffix_title, bio, email, phone,
      note
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
    cross join unnest(coalesce(cohort_ids, array[]::bigint[])) selected(cohort_id)
  ), proxy as (
    insert into user_proxy (person_id, user_id)
    select t_person.id, application.created_by
    from t_person cross join application
  )
  select * from t_person;
$$;

grant all on function confirm_membership_application to administrator;
