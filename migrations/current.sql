-- Write your migration here
drop view app_private.app_table_overview;
create or replace view app_private.app_table_overview as
  select
    c.relname,
    case c.relrowsecurity when true then '' else 'NO RLS' end as rls,
    case exists (select 1 from information_schema.columns where table_name = c.relname and table_schema = n.nspname and column_name='tenant')
      when true then '' else 'NO TENANT' end as has_tenant,
    case (select array_agg(grantee order by grantee asc)::text[]
          FROM information_schema.role_table_grants
          WHERE table_name = c.relname and table_schema = 'public' AND privilege_type = 'SELECT'
          group by table_name)
      when ARRAY['anonymous', 'olymp'] then null
      else
        (select array_agg(grantee order by grantee asc)
         from information_schema.role_table_grants
         where table_name = c.relname and table_schema = 'public' AND privilege_type = 'SELECT'
         group by table_name)
      end as wrong_acl,
    array(select p.polname from pg_policy p where p.polrelid = c.oid) as policies
from
    pg_class c
    join pg_namespace n on n.oid = c.relnamespace
  where
    c.relkind = 'r' and
    n.nspname in ('public')
  order by c.relname;

drop function if exists active_prospects();

grant all on function submit_form to anonymous;

select app_private.drop_policies('public.form_responses');
grant all on table form_responses to anonymous;
alter table form_responses enable row level security;
create policy admin_all on form_responses to administrator using (true) with check (true);

revoke all on cohort_group from administrator;
grant all on cohort_group to anonymous;
alter table cohort_group enable row level security;
select app_private.drop_policies('public.cohort_group');
create policy admin_all on cohort_group to administrator using (true) with check (true);
create policy all_view on cohort_group for select to anonymous;

revoke all on users from member;

revoke all on upozorneni from member;
grant all on upozorneni to anonymous;
alter table upozorneni enable row level security;
select app_private.drop_policies('public.upozorneni');
create policy admin_all on upozorneni to administrator using (true) with check (true);
create policy member_view on upozorneni for select to member;

revoke all on upozorneni_skupiny from member;
grant all on upozorneni_skupiny to anonymous;
alter table upozorneni_skupiny enable row level security;
select app_private.drop_policies('public.upozorneni_skupiny');
create policy admin_all on upozorneni_skupiny to administrator using (true) with check (true);
create policy member_view on upozorneni_skupiny for select to member;

revoke all on nabidka from member;
grant all on nabidka to anonymous;
select app_private.drop_policies('public.nabidka');
alter table nabidka enable row level security;
create policy admin_all on nabidka to administrator using (true) with check (true);
create policy member_view on nabidka for select to member;

revoke all on nabidka_item from member;
grant all on nabidka_item to anonymous;
select app_private.drop_policies('public.nabidka_item');
alter table nabidka_item enable row level security;
create policy admin_all on nabidka_item to administrator using (true) with check (true);
create policy member_view on nabidka_item for select to member;
create policy manage_own on nabidka_item for all to member
  using (ni_partner in (select current_couple_ids()))
  with check (ni_partner in (select current_couple_ids()));

revoke all on rozpis from member;
grant all on rozpis to anonymous;
select app_private.drop_policies('public.rozpis');
alter table rozpis enable row level security;
create policy admin_all on rozpis to administrator using (true) with check (true);
create policy member_view on rozpis for select to member;

revoke all on attachment from member;
grant all on attachment to anonymous;
select app_private.drop_policies('public.attachment');
alter table attachment enable row level security;
create policy admin_all on attachment to administrator using (true) with check (true);
create policy all_view on attachment for select to anonymous;

revoke all on rozpis_item from member;
grant all on rozpis_item to anonymous;
select app_private.drop_policies('public.rozpis_item');
create policy admin_all on rozpis_item to administrator using (true) with check (true);
create policy member_view on rozpis_item for select to member;
create policy manage_own on rozpis_item for all to member
  using (ri_partner in (select current_couple_ids()))
  with check (ri_partner in (select current_couple_ids()));

grant all on session to anonymous;

revoke all on room from administrator;
revoke all on room from member;
grant all on room to anonymous;
alter table room enable row level security;
select app_private.drop_policies('public.room');
create policy admin_all on room to administrator using (true) with check (true);
create policy all_view on room for select to anonymous;

revoke all on room_attachment from administrator;
revoke all on room_attachment from member;
grant all on room_attachment to anonymous;
alter table room_attachment enable row level security;
select app_private.drop_policies('public.room_attachment');
create policy admin_all on room_attachment to administrator using (true) with check (true);
create policy all_view on room_attachment for select to anonymous;

revoke all on location from administrator;
revoke all on location from member;
grant all on location to anonymous;
alter table location enable row level security;
select app_private.drop_policies('public.location');
create policy admin_all on location to administrator using (true) with check (true);
create policy all_view on location for select to anonymous;

revoke all on location_attachment from administrator;
revoke all on location_attachment from member;
grant all on location_attachment to anonymous;
alter table location_attachment enable row level security;
select app_private.drop_policies('public.location_attachment');
create policy admin_all on location_attachment to administrator using (true) with check (true);
create policy all_view on location_attachment for select to anonymous;

revoke all on tenant from administrator;
revoke all on tenant from member;
grant all on tenant to anonymous;
alter table tenant enable row level security;
select app_private.drop_policies('public.tenant');
create policy admin_all on tenant to administrator using (true) with check (true);
create policy all_view on tenant for select to anonymous;

revoke all on tenant_attachment from administrator;
revoke all on tenant_attachment from member;
grant all on tenant_attachment to anonymous;
alter table tenant_attachment enable row level security;
select app_private.drop_policies('public.tenant_attachment');
create policy admin_all on tenant_attachment to administrator using (true) with check (true);
create policy all_view on tenant_attachment for select to anonymous;

revoke all on tenant_person from administrator;
revoke all on tenant_person from member;
grant all on tenant_person to anonymous;
alter table tenant_person enable row level security;
select app_private.drop_policies('public.tenant_person');
create policy admin_all on tenant_person to administrator using (true) with check (true);
create policy all_view on tenant_person for select to anonymous;

revoke all on pary_navrh from member;
grant all on pary_navrh to anonymous;
select app_private.drop_policies('public.pary_navrh');
alter table pary_navrh enable row level security;
create policy admin_all on pary_navrh to administrator using (true) with check (true);
create policy manage_own on pary_navrh for all
  using (pn_navrhl = current_user_id() or pn_partner = current_user_id() or pn_partnerka = current_user_id())
  with check (pn_navrhl = current_user_id() and (pn_partner = current_user_id() or pn_partnerka = current_user_id()));

revoke all on platby_category from member;
grant all on platby_category to anonymous;
select app_private.drop_policies('public.platby_category');
alter table platby_category enable row level security;
create policy admin_all on platby_category to administrator using (true) with check (true);
create policy member_view on platby_category for select to member ;

revoke all on platby_category_group from member;
grant all on platby_category_group to anonymous;
select app_private.drop_policies('public.platby_category_group');
alter table platby_category_group enable row level security;
create policy admin_all on platby_category_group to administrator using (true) with check (true);
create policy member_view on platby_category_group for select to member ;

revoke all on platby_group from member;
grant all on platby_group to anonymous;
select app_private.drop_policies('public.platby_group');
alter table platby_group enable row level security;
create policy admin_all on platby_group to administrator using (true) with check (true);
create policy member_view on platby_group for select to member ;

revoke all on platby_group_skupina from member;
grant all on platby_group_skupina to anonymous;
select app_private.drop_policies('public.platby_group_skupina');
alter table platby_group_skupina enable row level security;
create policy admin_all on platby_group_skupina to administrator using (true) with check (true);
create policy member_view on platby_group_skupina for select to member ;

revoke all on platby_item from member;
grant all on platby_item to anonymous;
select app_private.drop_policies('public.platby_item');
alter table platby_item enable row level security;
create policy admin_all on platby_item to administrator using (true) with check (true);
create policy member_view on platby_item for select to member using (pi_id_user = current_user_id());

revoke all on platby_raw from member;
grant all on platby_raw to anonymous;
select app_private.drop_policies('public.platby_raw');
alter table platby_raw enable row level security;
create policy admin_all on platby_raw to administrator using (true) with check (true);
create policy member_view on platby_raw for select to member using (exists (select from platby_item where pi_id_raw = pr_id and pi_id_user = current_user_id()));

select app_private.drop_policies('public.dokumenty');
alter table dokumenty enable row level security;
create policy admin_all on dokumenty to administrator using (true) with check (true);
create policy all_view on dokumenty for select to member;
revoke all on dokumenty from member;
grant all on dokumenty to anonymous;

