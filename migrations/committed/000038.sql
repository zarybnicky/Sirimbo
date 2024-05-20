--! Previous: sha1:f2506e112be72df6619056b53bfee52a50d259de
--! Hash: sha1:d572ea3b5391f2334a731fd5f8e266a656f8f810

--! split: 1-current.sql
do $$
begin
  if exists (select 1 from pg_tables where schemaname = 'public' and tablename='skupiny') then
    alter table skupiny set schema app_private;
  end if;
end
$$;

GRANT ALL ON TABLE public.cohort TO anonymous;
ALTER TABLE public.cohort ENABLE ROW LEVEL SECURITY;
drop index if exists event_lesson_demand_registration_id_idx;
drop index if exists event_trainer_event_id_idx;

comment on column aktuality.at_kat is '@deprecated';
comment on column account.name is '@deprecated';
comment on column upozorneni.up_barvy is '@deprecated';
comment on column person.middle_name is '@deprecated';

create or replace function event_trainer_name(t event_trainer) returns text stable language sql
begin atomic
  select person_name(person.*) from person where t.person_id=person.id;
end;
grant all on function event_trainer_name to anonymous;

create or replace function event_instance_trainer_name(t event_instance_trainer) returns text stable language sql
begin atomic
  select person_name(person.*) from person where t.person_id=person.id;
end;
grant all on function event_instance_trainer_name to anonymous;

drop index if exists event_attendance_tenant_id_idx;
drop index if exists event_registration_tenant_id_idx;
drop index if exists event_instance_until_idx;
drop index if exists event_trainer_tenant_id_idx;
drop index if exists idx_pr_tenant;
drop index if exists idx_gf_tenant;
drop index if exists idx_pi_tenant;
drop index if exists event_lesson_demand_tenant_id_idx;
drop index if exists couple_man_id_idx;
drop index if exists couple_status_idx;
drop index if exists couple_woman_id_idx;
drop index if exists d_kategorie;
drop index if exists d_timestamp;
drop index if exists event_target_cohort_tenant_id_idx;
drop index if exists idx_24575_aktuality_at_foto_main_fkey;
drop index if exists idx_24593_dokumenty_d_kdo_fkey;
drop index if exists idx_24602_gd_id_rodic;
drop index if exists idx_24662_pary_navrh_pn_navrhl_fkey;
drop index if exists idx_24662_pary_navrh_pn_partner_fkey;
drop index if exists idx_24662_pary_navrh_pn_partnerka_fkey;
drop index if exists idx_24690_platby_category_group_pcg_id_category_fkey;
drop index if exists idx_24707_platby_group_skupina_pgs_id_group_fkey;
drop index if exists idx_24777_upozorneni_skupiny_ups_id_skupina_fkey;
drop index if exists is_public;
drop index if exists ordering;
drop index if exists idx_24613_gf_id_rodic;
drop index if exists idx_24713_platby_item_pi_id_category_fkey;
DROP INDEX if exists uploaded_by; -- 16 kB, Never Used Indexes, table attachment
DROP INDEX if exists idx_cg_tenant; -- 16 kB, Never Used Indexes, table cohort_group
DROP INDEX if exists idx_d_tenant; -- 16 kB, Never Used Indexes, table dokumenty
DROP INDEX if exists idx_fr_tenant; -- 16 kB, Never Used Indexes, table form_responses
DROP INDEX if exists type; -- 16 kB, Never Used Indexes, table form_responses
DROP INDEX if exists updated_at; -- 16 kB, Never Used Indexes, table form_responses
DROP INDEX if exists idx_gd_tenant; -- 16 kB, Never Used Indexes, table galerie_dir
DROP INDEX if exists idx_ot_tenant; -- 16 kB, Never Used Indexes, table otp_token
DROP INDEX if exists otp_token_user_id_idx; -- 16 kB, Never Used Indexes, table otp_token
DROP INDEX if exists app_private.idx_24662_pary_navrh_pn_navrhl_fkey; -- 16 kB, Never Used Indexes, table app_private.pary_navrh
DROP INDEX if exists app_private.idx_24662_pary_navrh_pn_partner_fkey; -- 16 kB, Never Used Indexes, table app_private.pary_navrh
DROP INDEX if exists app_private.idx_24662_pary_navrh_pn_partnerka_fkey; -- 16 kB, Never Used Indexes, table app_private.pary_navrh
DROP INDEX if exists idx_pei_tenant; -- 16 kB, Never Used Indexes, table person_invitation
DROP INDEX if exists person_invitation_person_id_idx; -- 16 kB, Never Used Indexes, table person_invitation
DROP INDEX if exists idx_pc_tenant; -- 16 kB, Never Used Indexes, table platby_category
DROP INDEX if exists idx_pcg_tenant; -- 16 kB, Never Used Indexes, table platby_category_group
DROP INDEX if exists idx_pg_tenant; -- 16 kB, Never Used Indexes, table platby_group
DROP INDEX if exists idx_pgs_tenant; -- 16 kB, Never Used Indexes, table platby_group_skupina
DROP INDEX if exists room_location_idx; -- 16 kB, Never Used Indexes, table room
DROP INDEX if exists app_private.idx_sk_tenant; -- 16 kB, Never Used Indexes, table app_private.skupiny
DROP INDEX if exists app_private.s_visible; -- 16 kB, Never Used Indexes, table app_private.skupiny
DROP INDEX if exists app_private.skupiny_cohort_group_idx; -- 16 kB, Never Used Indexes, table app_private.skupiny
DROP INDEX if exists app_private.skupiny_ordering_idx; -- 16 kB, Never Used Indexes, table app_private.skupiny
DROP INDEX if exists tenant_administrator_active_idx; -- 16 kB, Never Used Indexes, table tenant_administrator
DROP INDEX if exists tenant_administrator_status_idx; -- 16 kB, Never Used Indexes, table tenant_administrator
DROP INDEX if exists tenant_administrator_tenant_id_idx; -- 16 kB, Never Used Indexes, table tenant_administrator
DROP INDEX if exists tenant_location_tenant_id_idx; -- 16 kB, Never Used Indexes, table tenant_location
DROP INDEX if exists tenant_trainer_active_idx; -- 16 kB, Never Used Indexes, table tenant_trainer
DROP INDEX if exists tenant_trainer_status_idx; -- 16 kB, Never Used Indexes, table tenant_trainer
DROP INDEX if exists tenant_trainer_tenant_id_idx; -- 16 kB, Never Used Indexes, table tenant_trainer
DROP INDEX if exists user_proxy_active_idx; -- 16 kB, Never Used Indexes, table user_proxy
DROP INDEX if exists user_proxy_status_idx; -- 16 kB, Never Used Indexes, table user_proxy
DROP INDEX if exists idx_us_tenant; -- 16 kB, Never Used Indexes, table users


select app_private.drop_policies('public.person');

drop materialized view if exists auth_details;
create or replace view auth_details_view as
  SELECT
    person.id as person_id,
    array_remove(array_agg(couple.id), null) as couple_ids,
    array_remove(array_agg(cohort_id), null) as cohort_memberships,
    array_remove(array_agg(tenant_membership.tenant_id), null) tenant_memberships,
    array_remove(array_agg(tenant_trainer.tenant_id), null) tenant_trainers,
    array_remove(array_agg(tenant_administrator.tenant_id), null) tenant_administrators,
    array_remove(array_agg(tenant_administrator.tenant_id) || array_agg(tenant_trainer.tenant_id) || array_agg(tenant_membership.tenant_id), null) allowed_tenants --
  from person
  left join couple on (person.id=couple.man_id or person.id=couple.woman_id) and couple.status='active'
  left join cohort_membership on person.id=cohort_membership.person_id and cohort_membership.status='active'
  left join tenant_membership on person.id=tenant_membership.person_id and tenant_membership.status='active'
  left join tenant_trainer on person.id=tenant_trainer.person_id and tenant_trainer.status='active'
  left join tenant_administrator on person.id=tenant_administrator.person_id and tenant_administrator.status='active'
  group by person.id;
grant all on auth_details_view to anonymous;
comment on view auth_details_view is E'@omit';

create materialized view if not exists auth_details as
  SELECT * from auth_details_view;
create unique index on auth_details (person_id);
grant all on auth_details to anonymous;
comment on materialized view auth_details is E'@omit';

create or replace view allowed_tenants_view as
  SELECT
    person.id as person_id,
    tenant.id as tenant_id,
    max(member.id) is not null as is_member,
    max(trainer.id) is not null as is_trainer,
    max(admin.id) is not null as is_admin,
    max(member.id) is not null or max(trainer.id) is not null or max(admin.id) is not null as is_allowed
  from person
  cross join tenant
  left join tenant_membership member on member.tenant_id=tenant.id and member.person_id=person.id and member.status='active'
  left join tenant_trainer trainer on trainer.tenant_id=tenant.id and trainer.person_id=person.id and trainer.status='active'
  left join tenant_administrator admin on admin.tenant_id=tenant.id and admin.person_id=person.id and admin.status='active'
  where member.id is not null or trainer.id is not null or admin.id is not null
  group by person.id, tenant.id;
grant all on allowed_tenants_view to anonymous;
comment on view allowed_tenants_view is E'@omit';

create materialized view if not exists allowed_tenants as
  SELECT * from allowed_tenants_view;
create unique index on allowed_tenants (person_id, tenant_id);
grant all on allowed_tenants to anonymous;
comment on materialized view allowed_tenants is E'@omit';

drop function if exists event_instances_for_range(only_mine boolean, only_type public.event_type, start_range timestamp with time zone, end_range timestamp with time zone);
CREATE or replace FUNCTION public.event_instances_for_range(only_type public.event_type, start_range timestamp with time zone, end_range timestamp with time zone DEFAULT NULL::timestamp with time zone, only_mine boolean = false) RETURNS SETOF event_instance LANGUAGE sql STABLE
begin atomic
  select event_instance.*
  from event_instance join event on event_id=event.id
  where event.is_visible
    and tstzrange(start_range, end_range, '[]') && range
    and (only_type is null or event.type = only_type);
end;
COMMENT ON FUNCTION public.event_instances_for_range IS '@simpleCollections only';
GRANT ALL ON FUNCTION public.event_instances_for_range TO anonymous;

CREATE or replace FUNCTION public.my_event_instances_for_range(only_type public.event_type, start_range timestamp with time zone, end_range timestamp with time zone DEFAULT NULL::timestamp with time zone, only_mine boolean = false) RETURNS SETOF event_instance LANGUAGE sql STABLE
begin atomic
  select distinct on (event_instance.id) event_instance.*
  from event_instance
  join event on event_id=event.id
  left join event_registration on event_registration.event_id=event.id
  left join event_trainer on event_trainer.event_id=event.id
  left join event_instance_trainer on event_instance_trainer.instance_id=event_instance.id
  where event.is_visible
    and tstzrange(start_range, end_range, '[]') && range
    and (only_type is null or event.type = only_type)
    and (event_registration.person_id in (select my_person_ids())
      or event_registration.couple_id in (select my_couple_ids())
      or event_trainer.person_id in (select my_person_ids())
      or event_instance_trainer.person_id in (select my_person_ids()));
end;
COMMENT ON FUNCTION public.my_event_instances_for_range IS '@simpleCollections only';
GRANT ALL ON FUNCTION public.my_event_instances_for_range TO anonymous;

create or replace function app_private.create_latest_lesson_payments() returns setof payment language plpgsql volatile as $$
declare
  v_id bigint;
  created_ids bigint[] := array[]::bigint[];
begin
  perform set_config('jwt.claims.tenant_id', '2', true);
  perform set_config('jwt.claims.my_tenant_ids', '[2]', true);
  if not (select row_security_active('event')) then
    set local role to administrator;
  end if;

  update event set payment_type='after_instance'
  where type='lesson' and payment_type <> 'after_instance';

  select array_agg((create_event_instance_payment(event_instance)).id) into created_ids
  from event_instance join event on event.id=event_id
  where type='lesson'
    and not event_instance.is_cancelled
    and event_instance.since < now()
    and payment_type = 'after_instance'
    and not exists (
      select * from payment where event_instance_id=event_instance.id
    );

  update payment set status ='unpaid' where id = any (created_ids);

  foreach v_id in array created_ids loop
    perform resolve_payment_with_credit(payment.*) from payment where id = v_id;
  end loop;

  return query select * from payment where id = any (created_ids);
end;
$$;
select verify_function('app_private.create_latest_lesson_payments');

select app_private.drop_policies('public.cohort_subscription');
CREATE POLICY current_tenant ON cohort_subscription AS RESTRICTIVE USING (tenant_id = (select current_tenant_id()));
CREATE POLICY admin_manage ON cohort_subscription TO administrator USING (true);
CREATE POLICY member_view ON cohort_subscription for select to member USING (true);

select app_private.drop_policies('public.account');
CREATE POLICY current_tenant ON account AS RESTRICTIVE USING (tenant_id = (select current_tenant_id()));
CREATE POLICY admin_manage ON account TO administrator USING (true);
CREATE POLICY member_view ON account for select to member USING (true);

select app_private.drop_policies('public.accounting_period');
CREATE POLICY current_tenant ON accounting_period AS RESTRICTIVE USING (tenant_id = (select current_tenant_id()));
CREATE POLICY admin_manage ON accounting_period TO administrator USING (true);
CREATE POLICY public_view ON accounting_period for select USING (true);

select app_private.drop_policies('public.payment');
CREATE POLICY current_tenant ON payment AS RESTRICTIVE USING (tenant_id = (select current_tenant_id()));
CREATE POLICY admin_manage ON payment TO administrator USING (true);
CREATE POLICY member_view ON payment for select to member USING (true);

select app_private.drop_policies('public.payment_debtor');
CREATE POLICY current_tenant ON payment_debtor AS RESTRICTIVE USING (tenant_id = (select current_tenant_id()));
CREATE POLICY admin_manage ON payment_debtor TO administrator USING (true);
CREATE POLICY member_view ON payment_debtor for select to member USING (true);

select app_private.drop_policies('public.payment_recipient');
CREATE POLICY current_tenant ON payment_recipient AS RESTRICTIVE USING (tenant_id = (select current_tenant_id()));
CREATE POLICY admin_manage ON payment_recipient TO administrator USING (true);
CREATE POLICY member_view ON payment_recipient for select to member USING (true);

select app_private.drop_policies('public.transaction');
CREATE POLICY current_tenant ON transaction AS RESTRICTIVE USING (tenant_id = (select current_tenant_id()));
CREATE POLICY admin_manage ON transaction TO administrator USING (true);
CREATE POLICY member_view ON transaction for select to member USING (true);

select app_private.drop_policies('public.posting');
CREATE POLICY current_tenant ON posting AS RESTRICTIVE USING (tenant_id = (select current_tenant_id()));
CREATE POLICY admin_manage ON posting TO administrator USING (true);
CREATE POLICY member_view ON posting for select to member USING (true);

select app_private.drop_policies('public.event');
CREATE POLICY current_tenant ON event AS RESTRICTIVE USING (tenant_id = (select current_tenant_id()));
CREATE POLICY admin_same_tenant ON event to administrator USING (tenant_id in (select my_tenant_ids()));
CREATE POLICY trainer_same_tenant ON event to trainer
  USING (app_private.can_trainer_edit_event(id))
  WITH CHECK (tenant_id in (select my_tenant_ids()));
CREATE POLICY public_view ON event FOR SELECT USING (is_public = true or tenant_id in (select my_tenant_ids()));

select app_private.drop_policies('public.event_instance');
CREATE POLICY current_tenant ON event_instance AS RESTRICTIVE USING (tenant_id = (select current_tenant_id()));
CREATE POLICY admin_same_tenant ON event_instance to administrator USING (tenant_id in (select my_tenant_ids()));
CREATE POLICY trainer_same_tenant ON event_instance to trainer
  USING (app_private.can_trainer_edit_event(event_id))
  WITH CHECK (tenant_id in (select my_tenant_ids()));
CREATE POLICY view_visible_event ON event_instance FOR SELECT USING ((EXISTS ( SELECT 1
   FROM event
  WHERE (event_instance.event_id = event.id))));

select app_private.drop_policies('public.event_trainer');
CREATE POLICY current_tenant ON event_trainer AS RESTRICTIVE USING (tenant_id = (select current_tenant_id()));
create policy admin_all on event_trainer to administrator using (true);
CREATE POLICY trainer_same_tenant ON event_trainer to trainer
  USING (app_private.can_trainer_edit_event(event_id))
  WITH CHECK (tenant_id in (select my_tenant_ids()));
create policy member_view on event_trainer for select to member using (true);

select app_private.drop_policies('public.event_target_cohort');
create POLICY current_tenant on event_target_cohort as restrictive using (tenant_id = (select current_tenant_id()));
create policy admin_all on event_target_cohort to administrator using (true);
CREATE POLICY trainer_same_tenant ON event_target_cohort to trainer
  USING (app_private.can_trainer_edit_event(event_id))
  WITH CHECK (tenant_id in (select my_tenant_ids()));
create policy member_view on event_target_cohort for select to member using (true);

select app_private.drop_policies('public.event_instance_trainer');
create POLICY current_tenant on event_instance_trainer as restrictive using (tenant_id = (select current_tenant_id()));
create policy admin_all on event_instance_trainer to administrator using (true);
CREATE POLICY trainer_same_tenant ON event_instance_trainer to trainer
  USING (app_private.can_trainer_edit_event((select event_id from event_instance i where i.id = instance_id)))
  WITH CHECK (tenant_id in (select my_tenant_ids()));
create policy member_view on event_instance_trainer for select to member using (true);

select app_private.drop_policies('public.event_registration');
create policy admin_all on event_registration to administrator using (true);
CREATE POLICY trainer_same_tenant ON event_registration to trainer
  USING (app_private.can_trainer_edit_event(event_id))
  WITH CHECK (tenant_id in (select my_tenant_ids()));
create policy update_my on event_registration for update using (
  (select event_is_registration_open(event) from event where event_id = event.id)
  and (person_id in (select my_person_ids()) or couple_id in (select my_couple_ids()))
);
create policy delete_my on event_registration for delete using (
  (select event_is_registration_open(event) from event where event_id = event.id)
  and (person_id in (select my_person_ids()) or couple_id in (select my_couple_ids()))
);
create policy view_visible_event on event_registration for select using (
  exists (select 1 from event where event_id = event.id)
);

select app_private.drop_policies('public.tenant_location');
CREATE POLICY current_tenant ON tenant_location AS RESTRICTIVE USING (tenant_id = (select current_tenant_id()));
CREATE POLICY admin_all ON tenant_location TO administrator USING (true);
CREATE POLICY public_view ON tenant_location FOR SELECT using (true);

select app_private.drop_policies('public.tenant_attachment');
create policy current_tenant on tenant_attachment as restrictive using (tenant_id = current_tenant_id()) with check (tenant_id = current_tenant_id());
create policy admin_all on tenant_attachment to administrator using (true);
create policy public_view on tenant_attachment for select using (true);

select app_private.drop_policies('public.upozorneni_skupiny');
create policy current_tenant on upozorneni_skupiny as restrictive using (tenant_id = (select current_tenant_id()));
create policy admin_all on upozorneni_skupiny to administrator using (true) with check (true);
create policy member_view on upozorneni_skupiny for select to member using (true);

select app_private.drop_policies('public.membership_application');
CREATE POLICY current_tenant ON membership_application AS RESTRICTIVE USING (tenant_id = (select current_tenant_id()));
create policy manage_admin on membership_application to administrator using (true);
create policy manage_my on membership_application using (created_by = current_user_id());

select app_private.drop_policies('public.person_invitation');
CREATE POLICY current_tenant ON person_invitation AS RESTRICTIVE USING (tenant_id = (select current_tenant_id()));
CREATE POLICY admin_create on person_invitation using (
  exists (select 1 from tenant_administrator where tenant_administrator.person_id = any (current_person_ids()) and tenant_administrator.tenant_id = current_tenant_id())
);

select app_private.drop_policies('public.upozorneni');
create policy current_tenant on upozorneni as restrictive using (tenant_id = (select current_tenant_id()));
create policy admin_all on upozorneni to administrator using (true);
create policy member_view on upozorneni for select to member using (true);

select app_private.drop_policies('public.cohort_group');
create policy current_tenant on cohort_group as restrictive using (tenant_id = (select current_tenant_id()));
create policy admin_all on cohort_group to administrator using (true);
create policy public_view on cohort_group for select using (true);

select app_private.drop_policies('public.galerie_dir');
create policy current_tenant on galerie_dir as restrictive using (tenant_id = (select current_tenant_id()));
create policy admin_all on galerie_dir to administrator using (true);
create policy public_view on galerie_dir for select using (true);

select app_private.drop_policies('public.galerie_foto');
create policy current_tenant on galerie_foto as restrictive using (tenant_id = (select current_tenant_id()));
create policy admin_all on galerie_foto to administrator using (true);
create policy public_view on galerie_foto for select using (true);

select app_private.drop_policies('public.dokumenty');
create policy current_tenant on dokumenty as restrictive using (tenant_id = (select current_tenant_id()));
create policy admin_all on dokumenty to administrator using (true);
create policy member_view on dokumenty for select to member using (true);

select app_private.drop_policies('public.aktuality');
create policy current_tenant on aktuality as restrictive using (tenant_id = (select current_tenant_id()));
create policy admin_all on aktuality to administrator using (true);
create policy public_view on aktuality for select using (true);

select app_private.drop_policies('public.form_responses');
create policy current_tenant on form_responses as restrictive using (tenant_id = (select current_tenant_id()));
create policy admin_all on form_responses to administrator using (true);

select app_private.drop_policies('public.platby_item');
create policy current_tenant on platby_item as restrictive using (tenant_id = (select current_tenant_id()));
create policy admin_all on platby_item to administrator using (true);
create policy view_my on platby_item for select to member using (pi_id_user = current_user_id());

select app_private.drop_policies('public.platby_group');
create policy current_tenant on platby_group as restrictive using (tenant_id = (select current_tenant_id()));
create policy admin_all on platby_group to administrator using (true);
create policy member_view on platby_group for select to member using (true);

select app_private.drop_policies('public.platby_category_group');
create policy current_tenant on platby_category_group as restrictive using (tenant_id = (select current_tenant_id()));
create policy admin_all on platby_category_group to administrator using (true);
create policy member_view on platby_category_group for select to member using (true);

select app_private.drop_policies('public.platby_group_skupina');
create policy current_tenant on platby_group_skupina as restrictive using (tenant_id = (select current_tenant_id()));
create policy admin_all on platby_group_skupina to administrator using (true);
create policy member_view on platby_group_skupina for select to member using (true);

select app_private.drop_policies('public.platby_category');
create policy current_tenant on platby_category as restrictive using (tenant_id = (select current_tenant_id()));
create policy admin_all on platby_category to administrator using (true);
create policy member_view on platby_category for select to member using (true);

select app_private.drop_policies('public.platby_raw');
create policy current_tenant on platby_raw as restrictive using (tenant_id = (select current_tenant_id()));
create policy admin_all on platby_raw to administrator using (true);
create policy member_view on platby_raw for select to member using (exists (select from platby_item where pi_id_raw = pr_id and pi_id_user = current_user_id()));

select app_private.drop_policies('public.event_attendance');
create policy current_tenant on event_attendance as restrictive using (tenant_id = (select current_tenant_id()));
CREATE POLICY admin_all ON public.event_attendance TO administrator USING (true);
CREATE POLICY admin_trainer ON public.event_attendance TO trainer USING (exists (
  select 1
  from event_instance
  left join event_trainer on event_instance.event_id=event_trainer.event_id
  left join event_instance_trainer on event_instance.id=event_instance_trainer.instance_id
  where event_attendance.instance_id=event_instance.id and (
    event_instance_trainer.person_id in (select my_person_ids())
    or event_trainer.person_id in (select my_person_ids())
  )
));
CREATE POLICY view_visible_event ON public.event_attendance FOR SELECT USING ((EXISTS ( SELECT 1
   FROM public.event_instance
  WHERE (event_attendance.instance_id = event_instance.id))));

select app_private.drop_policies('public.person');
create policy admin_all on person to administrator using (true);
create policy admin_myself on person for update using (id in (select my_person_ids()));
create policy view_tenant_or_trainer on person for select using ((
    select (select current_tenant_id()) = any (allowed_tenants) and (
         (select current_tenant_id()) in (select my_tenant_ids())
      or (select current_tenant_id()) = any (tenant_trainers)
      or (select current_tenant_id()) = any (tenant_administrators)
    )
    from auth_details where person_id=id
));

create or replace function app_private.can_trainer_edit_event(eid bigint) returns boolean language sql as $$
  select (
    select count(person_id) > 0 from event_trainer where eid = event_id and person_id in (select my_person_ids())
  ) or not exists (
    select 1 from event_trainer where eid = event_id
  );
$$ security definer;
grant all on function app_private.can_trainer_edit_event to anonymous;

COMMENT ON FUNCTION public.current_user_id() IS '@omit';
COMMENT ON FUNCTION public.current_tenant_id() IS '@omit';
COMMENT ON FUNCTION public.current_couple_ids() IS '@omit';
COMMENT ON FUNCTION public.current_person_ids() IS '@omit';

drop function if exists current_session_id;
drop function if exists my_cohorts_array;
drop function if exists my_cohort_ids;
drop function if exists my_couples_array;
drop function if exists my_tenants_array;
drop function if exists my_persons_array;

CREATE TABLE if not exists "pghero_query_stats" (
  "id" bigserial primary key,
  "database" text,
  "user" text,
  "query" text,
  "query_hash" bigint,
  "total_time" float,
  "calls" bigint,
  "captured_at" timestamp
);
drop index if exists pghero_query_stats_database_captured_at_idx;
CREATE INDEX pghero_query_stats_database_captured_at_idx ON "pghero_query_stats" ("database", "captured_at");
comment on table pghero_query_stats is '@omit';

CREATE TABLE if not exists "pghero_space_stats" (
  "id" bigserial primary key,
  "database" text,
  "schema" text,
  "relation" text,
  "size" bigint,
  "captured_at" timestamp
);
drop index if exists pghero_space_stats_database_captured_at_idx;
CREATE INDEX pghero_space_stats_database_captured_at_idx ON "pghero_space_stats" ("database", "captured_at");
comment on table pghero_space_stats is '@omit';

--! split: 2-index_advisor.sql
create or replace function app_private.index_advisor(query text) returns table (
        startup_cost_before jsonb,
        startup_cost_after jsonb,
        total_cost_before jsonb,
        total_cost_after jsonb,
        index_statements text[],
        errors text[]
    )
    volatile
    language plpgsql
    as $$
declare
    n_args int;
    prepared_statement_name text = 'index_advisor_working_statement';
    hypopg_schema_name text = (select extnamespace::regnamespace::text from pg_extension where extname = 'hypopg');
    explain_plan_statement text;
    error_message text;
    rec record;
    plan_initial jsonb;
    plan_final jsonb;
    statements text[] = '{}';
    v_context text;
begin

    -- Remove comment lines (its common that they contain semicolons)
    query := trim(
        regexp_replace(
            regexp_replace(
                regexp_replace(query,'\/\*.+\*\/', '', 'g'),
            '--[^\r\n]*', ' ', 'g'),
        '\s+', ' ', 'g')
    );

    -- Remove trailing semicolon
    query := regexp_replace(query, ';\s*$', '');

    begin
        -- Disallow multiple statements
        if query ilike '%;%' then
            raise exception 'Query must not contain a semicolon';
        end if;

        -- Hack to support PostgREST because the prepared statement for args incorrectly defaults to text
        query := replace(query, 'WITH pgrst_payload AS (SELECT $1 AS json_data)', 'WITH pgrst_payload AS (SELECT $1::json AS json_data)');

        -- Create a prepared statement for the given query
        deallocate all;
        execute format('prepare %I as %s', prepared_statement_name, query);

        -- Detect how many arguments are present in the prepared statement
        n_args = (
            select
                coalesce(array_length(parameter_types, 1), 0)
            from
                pg_prepared_statements
            where
                name = prepared_statement_name
            limit
                1
        );

        -- Create a SQL statement that can be executed to collect the explain plan
        explain_plan_statement = format(
            'set local plan_cache_mode = force_generic_plan; explain (format json) execute %I%s',
            --'explain (format json) execute %I%s',
            prepared_statement_name,
            case
                when n_args = 0 then ''
                else format(
                    '(%s)', array_to_string(array_fill('null'::text, array[n_args]), ',')
                )
            end
        );
        -- Store the query plan before any new indexes
        execute explain_plan_statement into plan_initial;

        -- Create possible indexes
        for rec in (
            with extension_regclass as (
                select
                    distinct objid as oid
                from
                    pg_catalog.pg_depend
                where
                    deptype = 'e'
            )
            select
                pc.relnamespace::regnamespace::text as schema_name,
                pc.relname as table_name,
                pa.attname as column_name,
                format(
                    'select %I.hypopg_create_index($i$create index on %I.%I(%I)$i$)',
                    hypopg_schema_name,
                    pc.relnamespace::regnamespace::text,
                    pc.relname,
                    pa.attname
                ) hypopg_statement
            from
                pg_catalog.pg_class pc
                join pg_catalog.pg_attribute pa
                    on pc.oid = pa.attrelid
                left join extension_regclass er
                    on pc.oid = er.oid
                left join pg_catalog.pg_index pi
                    on pc.oid = pi.indrelid
                    and (select array_agg(x) from unnest(pi.indkey) v(x)) = array[pa.attnum]
                    and pi.indexprs is null -- ignore expression indexes
                    and pi.indpred is null -- ignore partial indexes
            where
                pc.relnamespace::regnamespace::text not in ( -- ignore schema list
                    'pg_catalog', 'pg_toast', 'information_schema'
                )
                and er.oid is null -- ignore entities owned by extensions
                and pc.relkind in ('r', 'm') -- regular tables, and materialized views
                and pc.relpersistence = 'p' -- permanent tables (not unlogged or temporary)
                and pa.attnum > 0
                and not pa.attisdropped
                and pi.indrelid is null
                and pa.atttypid in (20,16,1082,1184,1114,701,23,21,700,1083,2950,1700,25,18,1042,1043)
            )
            loop
                -- Create the hypothetical index
                execute rec.hypopg_statement;
            end loop;

        /*
        for rec in select * from hypopg()
            loop
                raise notice '%', rec;
            end loop;
        */

        -- Create a prepared statement for the given query
        -- The original prepared statement MUST be dropped because its plan is cached
        execute format('deallocate %I', prepared_statement_name);
        execute format('prepare %I as %s', prepared_statement_name, query);

        -- Store the query plan after new indexes
        execute explain_plan_statement into plan_final;

        --raise notice '%', plan_final;

        -- Idenfity referenced indexes in new plan
        execute format(
            'select
                coalesce(array_agg(hypopg_get_indexdef(indexrelid) order by indrelid, indkey::text), $i${}$i$::text[])
            from
                %I.hypopg()
            where
                %s ilike ($i$%%$i$ || indexname || $i$%%$i$)
            ',
            hypopg_schema_name,
            quote_literal(plan_final)::text
        ) into statements;

        -- Reset all hypothetical indexes
        perform hypopg_reset();

        -- Reset prepared statements
        deallocate all;

        return query values (
            (plan_initial -> 0 -> 'Plan' -> 'Startup Cost'),
            (plan_final -> 0 -> 'Plan' -> 'Startup Cost'),
            (plan_initial -> 0 -> 'Plan' -> 'Total Cost'),
            (plan_final -> 0 -> 'Plan' -> 'Total Cost'),
            statements::text[],
            array[]::text[]
        );
        return;

    exception when others then
        get stacked diagnostics error_message = MESSAGE_TEXT,
		v_context = pg_exception_context;

        return query values (
            null::jsonb,
            null::jsonb,
            null::jsonb,
            null::jsonb,
            array[]::text[],
            array[error_message, v_context]::text[]
        );
        return;
    end;

end;
$$;

comment on function app_private.index_advisor is '@omit';
