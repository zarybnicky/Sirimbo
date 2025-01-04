--! Previous: sha1:b5837979801984a5667a5b5dd3be0072ea455eb0
--! Hash: sha1:07e9d6c2d390b9ee2eedd7ee6edaa94c30c9e24e

--! split: 1-current.sql
select app_private.drop_policies('public.event_registration');
drop function if exists my_event_instances_for_range;
drop function if exists current_couple_ids;

CREATE or replace FUNCTION current_couple_ids() RETURNS bigint[] LANGUAGE sql STABLE LEAKPROOF PARALLEL SAFE AS $$
  SELECT translate(nullif(current_setting('jwt.claims.my_couple_ids', true), ''), '[]', '{}')::bigint[];
$$;

COMMENT ON FUNCTION current_couple_ids() IS '@omit';

GRANT ALL ON FUNCTION current_couple_ids() TO anonymous;

CREATE or replace FUNCTION current_person_ids() RETURNS bigint[] LANGUAGE sql STABLE LEAKPROOF PARALLEL SAFE AS $$
  SELECT translate(nullif(current_setting('jwt.claims.my_person_ids', true), '[]'), '[]', '{}')::bigint[] || array[]::bigint[];
$$;

COMMENT ON FUNCTION current_person_ids() IS '@omit';

GRANT ALL ON FUNCTION current_person_ids() TO anonymous;


create or replace function event_my_registrations(e event) returns setof event_registration language sql stable as $$
  select * from event_registration
  where event_id = e.id
  and (person_id = any (current_person_ids())
    or couple_id = any (current_couple_ids()));
$$;

COMMENT ON FUNCTION public.event_my_registrations IS '@simpleCollections only';

grant all on function event_my_registrations to anonymous;

CREATE or replace FUNCTION public.my_event_instances_for_range(only_type public.event_type, start_range timestamp with time zone, end_range timestamp with time zone DEFAULT NULL::timestamp with time zone, only_mine boolean = false) RETURNS SETOF event_instance LANGUAGE sql STABLE
begin atomic
  select distinct on (instances.id) instances.*
  from event_instances_for_range(only_type, start_range, end_range) instances
  left join event_registration on event_registration.event_id=instances.event_id and (event_registration.person_id = any(current_person_ids()) or event_registration.couple_id = any(current_couple_ids()))
  left join event_trainer on event_trainer.event_id=instances.event_id and event_trainer.person_id = any(current_person_ids())
  left join event_instance_trainer on event_instance_trainer.instance_id=instances.id and event_instance_trainer.person_id = any(current_person_ids())
  where event_registration.id is not null or event_trainer.id is not null or event_instance_trainer.id is not null;
end;
COMMENT ON FUNCTION public.my_event_instances_for_range IS '@simpleCollections only';
GRANT ALL ON FUNCTION public.my_event_instances_for_range TO anonymous;

create or replace function app_private.can_trainer_edit_event(eid bigint) returns boolean language sql as $$
  select exists (
    select 1 from event_trainer where eid = event_id and person_id = any (current_person_ids())
  ) or not exists (
    select 1 from event_trainer where eid = event_id
  );
$$ security definer stable leakproof parallel safe;

grant all on function app_private.can_trainer_edit_event to anonymous;

create or replace function cancel_registration(registration_id bigint) returns void language plpgsql as $$
declare
  v_event event;
  v_reg event_registration;
begin
  select * into v_reg from event_registration er where er.id = registration_id;
  select * into v_event from event where id = v_reg.event_id;

  if v_event is null or v_reg is null then
    raise exception 'EVENT_NOT_FOUND' using errcode = '28000';
  end if;
  if v_event.is_locked = true then
    raise exception 'NOT_ALLOWED' using errcode = '28000';
  end if;
  if v_reg.person_id <> all (current_person_ids()) and v_reg.couple_id <> all (current_couple_ids()) then
    raise exception 'ACCESS_DENIED' using errcode = '42501';
  end if;

  delete from event_registration where id = v_reg.id;
end;
$$;

select verify_function('public.cancel_registration');

GRANT ALL ON FUNCTION public.cancel_registration TO anonymous;

create or replace function edit_registration(registration_id bigint, note text) returns event_registration language plpgsql strict as $$
declare
  v_event event;
  reg event_registration;
begin
  select * into reg from event_registration where id = registration_id;
  select * into v_event from event where id = reg.event_id;

  if v_event is null or reg is null then
    raise exception 'EVENT_NOT_FOUND' using errcode = '28000';
  end if;
  if v_event.is_locked = true then
    raise exception 'NOT_ALLOWED' using errcode = '28000';
  end if;
  if reg.person_id <> all (current_person_ids()) and reg.couple_id <> all (current_couple_ids()) then
    raise exception 'ACCESS_DENIED' using errcode = '42501';
  end if;

  update event_registration set note=$2 where id = reg.id returning * into reg;
  return reg;
end;
$$;
select verify_function('public.edit_registration');
GRANT ALL ON FUNCTION public.edit_registration TO anonymous;

create or replace function register_to_event(inout registration event_registration, lessons event_lesson_demand[]) language plpgsql strict security definer as $$
declare
  event event;
  demand event_lesson_demand;
begin
  select * into event from event where id = registration.event_id;

  if event is null then
    raise exception 'EVENT_NOT_FOUND' using errcode = '28000';
  end if;
  if event.is_locked = true then
    raise exception 'NOT_ALLOWED' using errcode = '28000';
  end if;
  if registration.person_id <> all (current_person_ids()) and registration.couple_id <> all (current_couple_ids()) then
    raise exception 'ACCESS_DENIED' using errcode = '42501';
  end if;

  insert into event_registration (event_id, person_id, couple_id, note) select registration.event_id, registration.person_id, registration.couple_id, registration.note returning * into registration;
  foreach demand in array lessons loop
    perform set_lesson_demand(registration.id, demand.trainer_id, demand.lesson_count);
  end loop;
end;
$$;

comment on function register_to_event is E'@arg0variant create
@arg1variant patch';

select verify_function('public.register_to_event');

GRANT ALL ON FUNCTION public.register_to_event TO anonymous;

CREATE or replace FUNCTION register_to_event_many(registrations register_to_event_type[]) RETURNS SETOF event_registration
    LANGUAGE plpgsql
    AS $$
declare
  event event;
  created_ids bigint[] := array[]::bigint[];
  registration register_to_event_type;
  created event_registration;
  demand event_lesson_demand;
begin
  foreach registration in array registrations loop
    select * into event from event where id = registration.event_id;

    if event is null then
      raise exception 'EVENT_NOT_FOUND' using errcode = '28000';
    end if;
    if event.is_locked = true then
      raise exception 'NOT_ALLOWED' using errcode = '28000';
    end if;
    if registration.person_id <> all (current_person_ids()) and registration.couple_id <> all (current_couple_ids()) then
      raise exception 'ACCESS_DENIED' using errcode = '42501';
    end if;

    insert into event_registration (event_id, person_id, couple_id, note)
    values (registration.event_id, registration.person_id, registration.couple_id, registration.note)
    returning * into created;
    created_ids := created_ids || created.id;
    foreach demand in array registration.lessons loop
      perform set_lesson_demand(created.id, demand.trainer_id, demand.lesson_count);
    end loop;
  end loop;
  return query select * from event_registration where id = any (created_ids);
end;
$$ security definer;

select verify_function('public.register_to_event_many');
GRANT ALL ON FUNCTION public.register_to_event_many TO anonymous;


select app_private.drop_policies('public.event_attendance');
create policy current_tenant on event_attendance as restrictive using (tenant_id = (select current_tenant_id()));
CREATE POLICY admin_all ON event_attendance TO administrator USING (true);
CREATE POLICY admin_trainer ON event_attendance for update TO trainer USING (exists (
  select 1
  from event_instance
  left join event_trainer on event_instance.event_id=event_trainer.event_id
  left join event_instance_trainer on event_instance.id=event_instance_trainer.instance_id
  where event_attendance.instance_id=event_instance.id and (
    event_instance_trainer.person_id = any (current_person_ids())
    or event_trainer.person_id = any (current_person_ids())
  )
));
CREATE POLICY view_visible_event ON event_attendance FOR SELECT USING (EXISTS (SELECT 1
   FROM event_instance
  WHERE event_attendance.instance_id = event_instance.id));

select app_private.drop_policies('public.event_registration');
create policy admin_all on event_registration to administrator using (true);
CREATE POLICY trainer_same_tenant ON event_registration to trainer USING (app_private.can_trainer_edit_event(event_id)) WITH CHECK (true);
create policy update_my on event_registration for update using (
  (select event_is_registration_open(event) from event where event_id = event.id)
  and (person_id = any (current_person_ids()) or couple_id = any (current_couple_ids()))
);
create policy delete_my on event_registration for delete using (
  (select event_is_registration_open(event) from event where event_id = event.id)
  and (person_id = any (current_person_ids()) or couple_id = any (current_couple_ids()))
);
create policy view_visible_event on event_registration for select using (
  exists (select 1 from event where event_id = event.id)
);

select app_private.drop_policies('public.person');
create policy admin_all on person to administrator using (true);
create policy admin_myself on person for update using (id = any (current_person_ids()));
create policy view_tenant_or_trainer on person for select using ((
    select (select current_tenant_id()) = any (allowed_tenants) and (
         (select current_tenant_id()) in (select my_tenant_ids())
      or (select current_tenant_id()) = any (tenant_trainers)
      or (select current_tenant_id()) = any (tenant_administrators)
    )
    from auth_details where person_id=id
));


drop function if exists my_couple_ids;
drop function if exists my_person_ids;
