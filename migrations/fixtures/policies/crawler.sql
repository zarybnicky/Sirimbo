alter table crawler.frontier enable row level security;
select app_private.drop_policies('crawler.frontier');
create policy system_admin_read on crawler.frontier for select to system_admin using (true);

alter table crawler.json_response enable row level security;
select app_private.drop_policies('crawler.json_response');
create policy system_admin_read on crawler.json_response for select to system_admin using (true);

alter table crawler.json_response_cache enable row level security;
select app_private.drop_policies('crawler.json_response_cache');
create policy system_admin_read on crawler.json_response_cache for select to system_admin using (true);

alter table crawler.incremental_ranges enable row level security;
select app_private.drop_policies('crawler.incremental_ranges');
create policy system_admin_read on crawler.incremental_ranges for select to system_admin using (true);

alter table crawler.rate_limit_rule enable row level security;
select app_private.drop_policies('crawler.rate_limit_rule');
create policy system_admin_read on crawler.rate_limit_rule for select to system_admin using (true);

alter view crawler.frontier_failure set (security_invoker = true);
alter view crawler.frontier_summary set (security_invoker = true);
alter view crawler.frontier_failure_group set (security_invoker = true);
