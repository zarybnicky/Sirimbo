select app_private.drop_policies('public.event_lesson_demand');

create policy current_tenant on event_lesson_demand as restrictive
  using (tenant_id = (select current_tenant_id()));
create policy admin_all on event_lesson_demand to administrator
  using (true);
create policy view_visible_instance on event_lesson_demand for select
  using (exists (select 1 from event_instance_registration r where r.id = event_lesson_demand.registration_id));

grant all on event_lesson_demand to anonymous;
