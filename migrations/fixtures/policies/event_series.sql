select app_private.drop_policies('public.event_series');

create policy current_tenant on public.event_series as restrictive
  using (tenant_id = (select public.current_tenant_id()))
  with check (tenant_id = (select public.current_tenant_id()));
create policy admin_all on public.event_series to administrator
  using (true) with check (true);
create policy trainer_all on public.event_series to trainer
  using (true) with check (true);
create policy member_view on public.event_series for select to member
  using (exists (
    select 1 from public.event_instance instance
    where instance.series_id = event_series.id
  ));
create policy public_view on public.event_series for select to anonymous
  using (exists (
    select 1 from public.event_instance instance
    where instance.series_id = event_series.id
  ));
