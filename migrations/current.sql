-- Write your migration here
CREATE INDEX ON "public"."room_attachment"("object_name");
CREATE INDEX ON "public"."tenant_attachment"("object_name");
CREATE INDEX ON "public"."skupiny"("cohort_group");
CREATE INDEX ON "public"."skupiny"("ordering");
CREATE INDEX ON "public"."cohort_group"("tenant");
CREATE INDEX ON "public"."room"("location");

create or replace function current_tenant_id() returns bigint as $$
  select COALESCE(current_setting('jwt.claims.tenant_id', '1')::bigint, 1);
$$ language sql stable;
grant execute on function current_tenant_id to anonymous;

alter table upozorneni add column if not exists sticky boolean not null default false;


select app_private.drop_policies('public.attachment');
create policy admin_all on attachment to administrator using (true) with check (true);
create policy public_view on attachment for select to anonymous using (true);

select app_private.drop_policies('public.cohort_group');
create policy admin_all on cohort_group to administrator using (true) with check (true);
create policy public_view on cohort_group for select to anonymous using (true);
create policy my_tenant on cohort_group as restrictive using (tenant_id = current_tenant_id()) with check (tenant_id = current_tenant_id());

select app_private.drop_policies('public.dokumenty');
create policy admin_all on dokumenty to administrator using (true) with check (true);
create policy public_view on dokumenty for select to member using (true);
create policy my_tenant on dokumenty as restrictive using (tenant_id = current_tenant_id()) with check (tenant_id = current_tenant_id());

select app_private.drop_policies('public.location');
create policy admin_all on location to administrator using (true) with check (true);
create policy public_view on location for select to anonymous using (true);

select app_private.drop_policies('public.location_attachment');
create policy admin_all on location_attachment to administrator using (true) with check (true);
create policy public_view on location_attachment for select to anonymous using (true);

select app_private.drop_policies('public.nabidka');
create policy admin_all on nabidka to administrator using (true) with check (true);
create policy member_view on nabidka for select to member using (true);
create policy my_tenant on nabidka as restrictive using (tenant_id = current_tenant_id()) with check (tenant_id = current_tenant_id());

select app_private.drop_policies('public.nabidka_item');
create policy admin_all on nabidka_item to administrator using (true) with check (true);
create policy member_view on nabidka_item for select to member using (true);
create policy manage_own on nabidka_item for all to member
  using (ni_partner in (select current_couple_ids()))
  with check (ni_partner in (select current_couple_ids()));
create policy my_tenant on nabidka_item as restrictive using (tenant_id = current_tenant_id()) with check (tenant_id = current_tenant_id());

select app_private.drop_policies('public.platby_group');
create policy admin_all on platby_group to administrator using (true) with check (true);
create policy member_view on platby_group for select to member using (true);
create policy my_tenant on platby_group as restrictive using (tenant_id = current_tenant_id()) with check (tenant_id = current_tenant_id());

select app_private.drop_policies('public.platby_category');
alter table platby_category enable row level security;
create policy admin_all on platby_category to administrator using (true) with check (true);
create policy member_view on platby_category for select to member using (true);
create policy my_tenant on platby_category as restrictive using (tenant_id = current_tenant_id()) with check (tenant_id = current_tenant_id());

select app_private.drop_policies('public.platby_category_group');
create policy admin_all on platby_category_group to administrator using (true) with check (true);
create policy member_view on platby_category_group for select to member using (true);
create policy my_tenant on platby_category_group as restrictive using (tenant_id = current_tenant_id()) with check (tenant_id = current_tenant_id());

select app_private.drop_policies('public.platby_group_skupina');
create policy admin_all on platby_group_skupina to administrator using (true) with check (true);
create policy member_view on platby_group_skupina for select to member using (true);
create policy my_tenant on platby_group_skupina as restrictive using (tenant_id = current_tenant_id()) with check (tenant_id = current_tenant_id());

select app_private.drop_policies('public.room');
create policy admin_all on room to administrator using (true) with check (true);
create policy public_view on room for select to anonymous using (true);

select app_private.drop_policies('public.room_attachment');
create policy admin_all on room_attachment to administrator using (true) with check (true);
create policy public_view on room_attachment for select to anonymous using (true);

select app_private.drop_policies('public.rozpis');
create policy admin_all on rozpis to administrator using (true) with check (true);
create policy member_view on rozpis for select to member using (true);
create policy my_tenant on rozpis as restrictive using (tenant_id = current_tenant_id()) with check (tenant_id = current_tenant_id());

select app_private.drop_policies('public.rozpis_item');
create policy admin_all on rozpis_item to administrator using (true) with check (true);
create policy member_view on rozpis_item for select to member using (true);
create policy manage_own on rozpis_item for all to member
  using (ri_partner in (select current_couple_ids()))
  with check (ri_partner in (select current_couple_ids()));
create policy my_tenant on rozpis_item as restrictive using (tenant_id = current_tenant_id()) with check (tenant_id = current_tenant_id());

select app_private.drop_policies('public.tenant');
create policy admin_all on tenant to administrator using (true) with check (true);
create policy public_view on tenant for select to anonymous using (true);
create policy my_tenant on tenant as restrictive using (id = current_tenant_id()) with check (id = current_tenant_id());

select app_private.drop_policies('public.tenant_attachment');
create policy admin_all on tenant_attachment to administrator using (true) with check (true);
create policy public_view on tenant_attachment for select to anonymous using (true);
create policy my_tenant on tenant_attachment as restrictive using (tenant_id = current_tenant_id()) with check (tenant_id = current_tenant_id());

select app_private.drop_policies('public.tenant_location');
create policy admin_all on tenant_location to administrator using (true) with check (true);
create policy public_view on tenant_location for select to anonymous using (true);

select app_private.drop_policies('public.tenant_person');
create policy admin_all on tenant_person to administrator using (true) with check (true);
create policy public_view on tenant_person for select to anonymous using (true);
create policy my_tenant on tenant_person as restrictive using (tenant_id = current_tenant_id()) with check (tenant_id = current_tenant_id());

select app_private.drop_policies('public.upozorneni');
create policy admin_all on upozorneni to administrator using (true) with check (true);
create policy member_view on upozorneni for select to member using (true);
create policy my_tenant on upozorneni as restrictive using (tenant_id = current_tenant_id()) with check (tenant_id = current_tenant_id());

select app_private.drop_policies('public.upozorneni_skupiny');
create policy admin_all on upozorneni_skupiny to administrator using (true) with check (true);
create policy member_view on upozorneni_skupiny for select to member using (true);
create policy my_tenant on upozorneni_skupiny as restrictive using (tenant_id = current_tenant_id()) with check (tenant_id = current_tenant_id());
