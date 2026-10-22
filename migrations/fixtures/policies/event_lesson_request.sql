select app_private.drop_policies('public.event_lesson_request');

create policy current_tenant on event_lesson_request as restrictive
  using (tenant_id = (select current_tenant_id()));
create policy admin_all on event_lesson_request to administrator
  using (true);
create policy view_visible_instance on event_lesson_request for select
  using (exists (select 1 from event_registration r where r.id = event_lesson_request.registration_id));

grant all on event_lesson_request to anonymous;
