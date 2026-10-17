select app_private.drop_policies('public.scoreboard_manual_adjustment');

create policy current_tenant on scoreboard_manual_adjustment as restrictive
  using (tenant_id = (select current_tenant_id()));
create policy admin_all on scoreboard_manual_adjustment to administrator
  using (true);
create policy member_read on scoreboard_manual_adjustment for select
  using (true);

