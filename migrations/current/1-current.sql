do $$
begin
  if not exists (select * from pg_type where typcategory='E' and typname = 'relationship_status') then
    create type relationship_status as enum (
      'expired',
      'active',
      'pending'
    );
  end if;
end
$$;

alter table user_proxy add column if not exists status relationship_status not null default 'active';
alter table couple add column if not exists status relationship_status not null default 'active';
alter table cohort_membership add column if not exists status relationship_status not null default 'active';
alter table tenant_membership add column if not exists status relationship_status not null default 'active';
alter table tenant_trainer add column if not exists status relationship_status not null default 'active';
alter table tenant_administrator add column if not exists status relationship_status not null default 'active';

create or replace function app_private.cron_update_memberships() returns void language sql as $$
  update user_proxy set status = 'active' where now() <@ active_range and status <> 'active';
  update user_proxy set status = 'expired' where now() < since and status <> 'expired';
  update user_proxy set status = 'pending' where now() > until and status <> 'pending';

  update couple set status = 'active' where now() <@ active_range and status <> 'active';
  update couple set status = 'expired' where now() < since and status <> 'expired';
  update couple set status = 'pending' where now() > until and status <> 'pending';

  update cohort_membership set status = 'active' where now() <@ active_range and status <> 'active';
  update cohort_membership set status = 'expired' where now() < since and status <> 'expired';
  update cohort_membership set status = 'pending' where now() > until and status <> 'pending';

  update tenant_membership set status = 'active' where now() <@ active_range and status <> 'active';
  update tenant_membership set status = 'expired' where now() < since and status <> 'expired';
  update tenant_membership set status = 'pending' where now() > until and status <> 'pending';

  update tenant_trainer set status = 'active' where now() <@ active_range and status <> 'active';
  update tenant_trainer set status = 'expired' where now() < since and status <> 'expired';
  update tenant_trainer set status = 'pending' where now() > until and status <> 'pending';

  update tenant_administrator set status = 'active' where now() <@ active_range and status <> 'active';
  update tenant_administrator set status = 'expired' where now() < since and status <> 'expired';
  update tenant_administrator set status = 'pending' where now() > until and status <> 'pending';
$$;

grant all on function app_private.cron_update_memberships to administrator;

create or replace function app_private.tg_tenant_membership__on_status() returns trigger language plpgsql as $$
begin
  if NEW.status = 'expired' then
    update cohort_membership set status = 'expired', until = NEW.until where cohort_membership.person_id = NEW.person_id;
  end if;
  return NEW;
end;
$$;
select verify_function('app_private.tg_tenant_membership__on_status', 'tenant_membership');

CREATE or replace TRIGGER _500_on_status
  after UPDATE ON tenant_membership
  FOR EACH row
  WHEN (OLD.status IS DISTINCT FROM NEW.status)
  EXECUTE PROCEDURE app_private.tg_tenant_membership__on_status();

create or replace function app_private.tg_cohort_membership__on_status() returns trigger language plpgsql as $$
begin
  if NEW.status = 'expired' and (TG_OP = 'INSERT' or OLD.status <> NEW.status) then
    -- remove event_registrations for future events
    -- remove event_attendance for ongoing events
  elsif NEW.status = 'active' and (TG_OP = 'INSERT' or OLD.status <> NEW.status) then
    -- add payments
    -- add event_registrations to cohort events
  end if;
  return NEW;
end;
$$;
select verify_function('app_private.tg_cohort_membership__on_status', 'cohort_membership');

CREATE or replace TRIGGER _500_on_status
  after insert or UPDATE ON cohort_membership
  FOR EACH row
  EXECUTE PROCEDURE app_private.tg_cohort_membership__on_status();


create or replace function app_private.can_trainer_edit_event(eid bigint) returns boolean language sql as $$
  select (
    select count(person_id) > 0 from event_trainer where eid = event_id and person_id = any (my_persons_array())
  ) or not exists (
    select 1 from event_trainer where eid = event_id
  );
$$ security definer;
grant all on function app_private.can_trainer_edit_event to anonymous;


select app_private.drop_policies('public.event');
CREATE POLICY my_tenant ON public.event AS RESTRICTIVE USING (tenant_id = current_tenant_id());
CREATE POLICY admin_same_tenant ON public.event to administrator USING (tenant_id = any (my_tenants_array()));
CREATE POLICY trainer_same_tenant ON public.event to trainer
  USING (app_private.can_trainer_edit_event(id))
  WITH CHECK (tenant_id = any (my_tenants_array()));
CREATE POLICY view_public ON public.event FOR SELECT TO anonymous USING (is_public = true or tenant_id = any (my_tenants_array()));

select app_private.drop_policies('public.event_instance');
CREATE POLICY my_tenant ON public.event_instance AS RESTRICTIVE USING (tenant_id = current_tenant_id());
CREATE POLICY admin_same_tenant ON public.event_instance to administrator USING (tenant_id = any (my_tenants_array()));
CREATE POLICY trainer_same_tenant ON public.event_instance to trainer
  USING (app_private.can_trainer_edit_event(event_id))
  WITH CHECK (tenant_id = any (my_tenants_array()));
CREATE POLICY view_visible_event ON public.event_instance FOR SELECT USING ((EXISTS ( SELECT 1
   FROM public.event
  WHERE (event_instance.event_id = event.id))));

select app_private.drop_policies('public.event_trainer');
CREATE POLICY my_tenant ON public.event_trainer AS RESTRICTIVE USING (tenant_id = current_tenant_id());
create policy admin_all on event_trainer to administrator using (true);
CREATE POLICY trainer_same_tenant ON public.event_trainer to trainer
  USING (app_private.can_trainer_edit_event(event_id))
  WITH CHECK (tenant_id = any (my_tenants_array()));
create policy view_all on event_trainer for select to member using (true);

select app_private.drop_policies('public.event_target_cohort');
create policy my_tenant on event_target_cohort as restrictive using (tenant_id = current_tenant_id());
create policy admin_all on event_target_cohort to administrator using (true);
CREATE POLICY trainer_same_tenant ON public.event_target_cohort to trainer
  USING (app_private.can_trainer_edit_event(event_id))
  WITH CHECK (tenant_id = any (my_tenants_array()));
create policy view_tenant on event_target_cohort for select to member using (true);

select app_private.drop_policies('public.event_instance_trainer');
create policy my_tenant on event_instance_trainer as restrictive using (tenant_id = current_tenant_id());
create policy admin_all on event_instance_trainer to administrator using (true);
CREATE POLICY trainer_same_tenant ON public.event_instance_trainer to trainer
  USING (app_private.can_trainer_edit_event((select event_id from event_instance i where i.id = instance_id)))
  WITH CHECK (tenant_id = any (my_tenants_array()));
create policy view_tenant on event_instance_trainer for select to member using (true);

select app_private.drop_policies('public.event_registration');
create policy admin_all on event_registration to administrator using (true);
CREATE POLICY trainer_same_tenant ON public.event_registration to trainer
  USING (app_private.can_trainer_edit_event(event_id))
  WITH CHECK (tenant_id = any (my_tenants_array()));
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
