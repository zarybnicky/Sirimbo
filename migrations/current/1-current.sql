-- Write your migration here

-- insert into event_registration (event_id, target_cohort_id, person_id)
-- select event.id, event_target_cohort.id, cohort_membership.person_id
-- from event
-- join event_target_cohort on event_id=event.id
-- join cohort_membership on event_target_cohort.cohort_id=cohort_membership.cohort_id and active_range @> now()
-- left join event_registration on target_cohort_id=event_target_cohort.id and event_registration.person_id=cohort_membership.person_id and event_registration.event_id=event.id
-- where cohort_membership.cohort_id in (2,11) and event_registration.id is null
-- and event.id in (select distinct event_id from event_instances_for_range(false, null, '2023-09-01') where tenant_id=1)
-- on conflict on constraint event_registration_unique_event_person_couple_key do nothing;

select app_private.drop_policies('public.cohort_subscription');
CREATE POLICY admin_manage ON public.cohort_subscription TO administrator USING (true);
CREATE POLICY person_view ON public.cohort_subscription for select TO anonymous USING (true);
CREATE POLICY my_tenant ON public.cohort_subscription AS RESTRICTIVE USING ((tenant_id = public.current_tenant_id()));

select app_private.drop_policies('public.account');
CREATE POLICY admin_manage ON public.account TO administrator USING (true);
CREATE POLICY person_view ON public.account for select TO anonymous USING (true);
CREATE POLICY my_tenant ON public.account AS RESTRICTIVE USING ((tenant_id = public.current_tenant_id()));

select app_private.drop_policies('public.accounting_period');
CREATE POLICY admin_manage ON public.accounting_period TO administrator USING (true);
CREATE POLICY person_view ON public.accounting_period for select TO anonymous USING (true);
CREATE POLICY my_tenant ON public.accounting_period AS RESTRICTIVE USING ((tenant_id = public.current_tenant_id()));

select app_private.drop_policies('public.payment');
CREATE POLICY admin_manage ON public.payment TO administrator USING (true);
CREATE POLICY person_view ON public.payment for select TO anonymous USING (true);
CREATE POLICY my_tenant ON public.payment AS RESTRICTIVE USING ((tenant_id = public.current_tenant_id()));

select app_private.drop_policies('public.payment_debtor');
CREATE POLICY admin_manage ON public.payment_debtor TO administrator USING (true);
CREATE POLICY person_view ON public.payment_debtor for select TO anonymous USING (true);
CREATE POLICY my_tenant ON public.payment_debtor AS RESTRICTIVE USING ((tenant_id = public.current_tenant_id()));

select app_private.drop_policies('public.payment_recipient');
CREATE POLICY admin_manage ON public.payment_recipient TO administrator USING (true);
CREATE POLICY person_view ON public.payment_recipient for select TO anonymous USING (true);
CREATE POLICY my_tenant ON public.payment_recipient AS RESTRICTIVE USING ((tenant_id = public.current_tenant_id()));

select app_private.drop_policies('public.transaction');
CREATE POLICY admin_manage ON public.transaction TO administrator USING (true);
CREATE POLICY person_view ON public.transaction for select TO anonymous USING (true);
CREATE POLICY my_tenant ON public.transaction AS RESTRICTIVE USING ((tenant_id = public.current_tenant_id()));

select app_private.drop_policies('public.posting');
CREATE POLICY admin_manage ON public.posting TO administrator USING (true);
CREATE POLICY person_view ON public.posting for select TO anonymous USING (true);
CREATE POLICY my_tenant ON public.posting AS RESTRICTIVE USING ((tenant_id = public.current_tenant_id()));

grant all on function payment_debtor_price to anonymous;

create or replace function current_session_id() returns text as $$
  select nullif(current_setting('jwt.claims.user_id', true), '')::integer;
$$ language sql stable;
grant execute on function current_session_id to anonymous;

create or replace function current_user_id() returns bigint as $$
  SELECT nullif(current_setting('jwt.claims.user_id', true), '')::bigint;
$$ language sql stable;
grant execute on function current_user_id to anonymous;

drop function if exists app_private.log_in_as;
create or replace function app_private.log_in_as(u users) returns table (key text, value text) language sql as $$
  select 'jwt.claims.' || kv.key, set_config('jwt.claims.' || kv.key, kv.value, false)
  from app_private.create_jwt_token(u) j join lateral jsonb_each_text(to_jsonb(j)) kv on true
  union
  select 'role', set_config('role', case when is_admin then 'administrator' when is_trainer then 'trainer' when is_member then 'member' else 'anonymous' end, false)
  from app_private.create_jwt_token(u) j
$$;
