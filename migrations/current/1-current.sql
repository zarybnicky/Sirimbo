drop index if exists event_lesson_demand_registration_id_idx;
CREATE INDEX event_lesson_demand_registration_id_idx ON public.event_lesson_demand USING btree (registration_id);

drop index if exists event_trainer_event_idx;
CREATE INDEX event_trainer_event_idx ON public.event_trainer USING btree (event_id, person_id);
drop index if exists event_trainer_tenant_event_idx;
CREATE INDEX event_trainer_tenant_event_idx ON public.event_trainer USING btree (tenant_id, event_id);

drop index if exists idx_event_tenant;
CREATE INDEX idx_event_tenant ON public.event USING btree (tenant_id, is_visible);

drop index if exists event_instance_tenant_id_idx;
drop index if exists event_instance_range_idx;
CREATE INDEX event_instance_range_idx ON public.event_instance (tenant_id, since, until);

select app_private.drop_policies('public.event');
CREATE POLICY current_tenant ON event AS RESTRICTIVE USING (tenant_id = (select current_tenant_id()));
CREATE POLICY admin_same_tenant ON event to administrator USING (true);
CREATE POLICY trainer_same_tenant ON event to trainer USING (app_private.can_trainer_edit_event(id)) WITH CHECK (true);
CREATE POLICY member_view ON event FOR SELECT to member USING (is_visible);
CREATE POLICY public_view ON event FOR SELECT to anonymous USING (is_public);

select app_private.drop_policies('public.event_instance');
CREATE POLICY current_tenant ON event_instance AS RESTRICTIVE USING (tenant_id = (select current_tenant_id()));
CREATE POLICY admin_same_tenant ON event_instance to administrator USING (true);
CREATE POLICY trainer_same_tenant ON event_instance to trainer USING (app_private.can_trainer_edit_event(event_id)) WITH CHECK (true);
CREATE POLICY view_visible_event ON event_instance FOR SELECT USING ((EXISTS ( SELECT 1
   FROM event
  WHERE (event_instance.event_id = event.id))));

select app_private.drop_policies('public.event_trainer');
CREATE POLICY current_tenant ON event_trainer AS RESTRICTIVE USING (tenant_id = (select current_tenant_id()));
create policy admin_all on event_trainer to administrator using (true);
CREATE POLICY trainer_same_tenant ON event_trainer to trainer USING (app_private.can_trainer_edit_event(event_id)) WITH CHECK (true);
create policy member_view on event_trainer for select to member using (true);

select app_private.drop_policies('public.event_target_cohort');
create POLICY current_tenant on event_target_cohort as restrictive using (tenant_id = (select current_tenant_id()));
create policy admin_all on event_target_cohort to administrator using (true);
CREATE POLICY trainer_same_tenant ON event_target_cohort to trainer USING (app_private.can_trainer_edit_event(event_id)) WITH CHECK (true);
create policy member_view on event_target_cohort for select to member using (true);

select app_private.drop_policies('public.event_instance_trainer');
create POLICY current_tenant on event_instance_trainer as restrictive using (tenant_id = (select current_tenant_id()));
create policy admin_all on event_instance_trainer to administrator using (true);
CREATE POLICY trainer_same_tenant ON event_instance_trainer to trainer USING (app_private.can_trainer_edit_event((select event_id from event_instance i where i.id = instance_id))) WITH CHECK (true);
create policy member_view on event_instance_trainer for select to member using (true);

select app_private.drop_policies('public.event_registration');
create policy admin_all on event_registration to administrator using (true);
CREATE POLICY trainer_same_tenant ON event_registration to trainer USING (app_private.can_trainer_edit_event(event_id)) WITH CHECK (true);
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

CREATE or replace FUNCTION public.event_instances_for_range(only_type public.event_type, start_range timestamp with time zone, end_range timestamp with time zone DEFAULT NULL::timestamp with time zone, only_mine boolean = false) RETURNS SETOF event_instance LANGUAGE sql STABLE
begin atomic
  select event_instance.*
  from event_instance
  join event on event_id=event.id
  where event.is_visible
    and event_instance.since <= end_range
    and event_instance.until >= start_range
    and (only_type is null or event.type = only_type);
end;
COMMENT ON FUNCTION public.event_instances_for_range IS '@simpleCollections only';
GRANT ALL ON FUNCTION public.event_instances_for_range TO anonymous;

CREATE or replace FUNCTION public.my_event_instances_for_range(only_type public.event_type, start_range timestamp with time zone, end_range timestamp with time zone DEFAULT NULL::timestamp with time zone, only_mine boolean = false) RETURNS SETOF event_instance LANGUAGE sql STABLE
begin atomic
  select distinct on (instances.id) instances.*
  from event_instances_for_range(only_type, start_range, end_range) instances
  left join event_registration on event_registration.event_id=instances.event_id and (event_registration.person_id = any(array(select my_person_ids())) or event_registration.couple_id = any(array(select my_couple_ids())))
  left join event_trainer on event_trainer.event_id=instances.event_id and event_trainer.person_id = any(array(select my_person_ids()))
  left join event_instance_trainer on event_instance_trainer.instance_id=instances.id and  event_instance_trainer.person_id = any(array(select my_person_ids()))
  where event_registration.id is not null or event_trainer is not null or event_instance_trainer is not null;
end;
COMMENT ON FUNCTION public.my_event_instances_for_range IS '@simpleCollections only';
GRANT ALL ON FUNCTION public.my_event_instances_for_range TO anonymous;

drop function if exists log_in_as;
CREATE FUNCTION public.log_in_as(id bigint, OUT usr public.users, OUT jwt public.jwt_token) RETURNS record
    LANGUAGE plpgsql STRICT SECURITY DEFINER
    AS $$
begin
  select users.* into usr from users where users.id=$1;
  jwt := app_private.create_jwt_token(usr);
end
$$;
grant execute on function log_in_as to administrator;
--select verify_function('log_in_as');
