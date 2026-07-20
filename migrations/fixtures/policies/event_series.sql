select app_private.drop_policies('public.event_series');

create policy current_tenant on event_series as restrictive
  using (tenant_id = (select current_tenant_id()));
create policy admin_all on event_series to administrator
  using (true);
create policy trainer_all on event_series to trainer
  using (true);
create policy member_view on event_series for select to member
  using (exists (select 1 from event_instance instance where instance.series_id = event_series.id));
create policy public_view on event_series for select to anonymous
  using (exists (select 1 from event_instance instance where instance.series_id = event_series.id));
