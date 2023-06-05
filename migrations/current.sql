-- Write your migration here
drop view app_private.app_table_overview;
create or replace view app_private.app_table_overview as
  select
    c.relname,
    case c.relrowsecurity when true then '' else 'NO RLS' end as rls,
    case exists (
      select 1
      from information_schema.columns
      where table_name = c.relname and table_schema = n.nspname and column_name='id'
    )
      when true then ''
      else 'NO ID' end as has_id,
    case exists (
      select 1
      from information_schema.columns
      where table_name = c.relname and table_schema = n.nspname and column_name='tenant_id'
    )
      when true then ''
      else 'NO TENANT' end as has_tenant,
    case (
      select array_agg(grantee order by grantee asc)::text[]
      FROM information_schema.role_table_grants
      WHERE table_name = c.relname and table_schema = 'public' AND privilege_type = 'SELECT'
      group by table_name
    )
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

create or replace function public.users_has_valid_payment(a public.users) returns boolean
    language sql stable as $$
  SELECT EXISTS (
    SELECT pi_id
    FROM platby_item
      INNER JOIN platby_category ON pi_id_category=pc_id
      INNER JOIN platby_category_group ON pcg_id_category=pc_id
      INNER JOIN platby_group ON pg_id=pcg_id_group
    WHERE pg_type='1'
      AND CURRENT_DATE >= pc_valid_from
      AND CURRENT_DATE <= pc_valid_to
      AND pi_id_user = a.u_id
  )
$$;
grant all on function public.users_has_valid_payment to anonymous;

create or replace function public.users_date_of_newest_payment(a public.users) returns date
    language sql stable as $$
  SELECT max(pi_date)
  FROM platby_item
  where pi_id_user = a.u_id
$$ security definer;
grant all on function public.users_date_of_newest_payment to anonymous;

create or replace function public.users_date_of_oldest_payment(a public.users) returns date
    language sql stable as $$
  SELECT min(pi_date)
  FROM platby_item
  where pi_id_user = a.u_id
$$ security definer;
grant all on function public.users_date_of_oldest_payment to anonymous;

create or replace function public.users_in_public_cohort(a public.users) returns boolean
    language sql stable as $$
  SELECT s_visible
  FROM skupiny
  inner join users on s_id = u_skupina
  where u_id = a.u_id
$$;
comment on function users_in_public_cohort(public.users) is E'@filterable';
grant all on function public.users_in_public_cohort to anonymous;

drop view if exists members;

grant all on session to anonymous;
comment on table session is E'@omit create,update,delete';

alter table permissions add column if not exists id bigint generated always as (pe_id) stored;

alter table aktuality add column if not exists id bigint generated always as (at_id) stored;
alter table aktuality add column if not exists tenant_id bigint not null default current_tenant_id();
CREATE INDEX if not exists "tenant_id" ON aktuality (tenant_id);
select app_private.drop_policies('public.aktuality');
create policy admin_all on aktuality to administrator using (true) with check (true);
create policy all_view on aktuality for select using (true);
create policy my_tenant on aktuality as restrictive using (tenant_id = current_tenant_id()) with check (tenant_id = current_tenant_id());

alter table aktuality add column if not exists id bigint generated always as (at_id) stored;
alter table aktuality add column if not exists tenant_id bigint not null default current_tenant_id();
CREATE INDEX if not exists "tenant_id" ON aktuality (tenant_id);
select app_private.drop_policies('public.aktuality');
create policy admin_all on aktuality to administrator using (true) with check (true);
create policy all_view on aktuality for select using (true);
create policy my_tenant on aktuality as restrictive using (tenant_id = current_tenant_id()) with check (tenant_id = current_tenant_id());

alter table attendee_external add column if not exists tenant_id bigint not null default current_tenant_id();
CREATE INDEX if not exists "tenant_id" ON attendee_external (tenant_id);
select app_private.drop_policies('public.attendee_external');
create policy admin_all on attendee_external to administrator using (true) with check (true);
create policy select_member on attendee_external for select to member using (true);
create policy my_tenant on attendee_external as restrictive using (tenant_id = current_tenant_id()) with check (tenant_id = current_tenant_id());

alter table attendee_user add column if not exists tenant_id bigint not null default current_tenant_id();
CREATE INDEX if not exists "tenant_id" ON attendee_user (tenant_id);
select app_private.drop_policies('public.attendee_user');
create policy admin_all on attendee_user to administrator using (true) with check (true);
create policy select_member on attendee_user for select to member using (true);
CREATE POLICY manage_own ON attendee_user TO member USING (user_id = current_user_id()) WITH CHECK (user_id = current_user_id());
create policy my_tenant on attendee_user as restrictive using (tenant_id = current_tenant_id()) with check (tenant_id = current_tenant_id());

revoke all on attachment from member;
grant all on attachment to anonymous;
select app_private.drop_policies('public.attachment');
alter table attachment enable row level security;
create policy admin_all on attachment to administrator using (true) with check (true);
create policy public_view on attachment for select to anonymous;

alter table cohort_group add column if not exists tenant_id bigint not null default current_tenant_id();
CREATE INDEX if not exists "tenant_id" ON cohort_group (tenant_id);
revoke all on cohort_group from administrator;
grant all on cohort_group to anonymous;
alter table cohort_group enable row level security;
select app_private.drop_policies('public.cohort_group');
create policy admin_all on cohort_group to administrator using (true) with check (true);
create policy public_view on cohort_group for select to anonymous;
create policy my_tenant on cohort_group as restrictive using (tenant_id = current_tenant_id()) with check (tenant_id = current_tenant_id());

alter table dokumenty add column if not exists id bigint generated always as (d_id) stored;
alter table dokumenty add column if not exists tenant_id bigint not null default current_tenant_id();
CREATE INDEX if not exists "tenant_id" ON dokumenty (tenant_id);
select app_private.drop_policies('public.dokumenty');
alter table dokumenty enable row level security;
create policy admin_all on dokumenty to administrator using (true) with check (true);
create policy public_view on dokumenty for select to member;
create policy my_tenant on dokumenty as restrictive using (tenant_id = current_tenant_id()) with check (tenant_id = current_tenant_id());
revoke all on dokumenty from member;
grant all on dokumenty to anonymous;

alter table galerie_dir add column if not exists id bigint generated always as (gd_id) stored;
alter table galerie_dir add column if not exists tenant_id bigint not null default current_tenant_id();
CREATE INDEX if not exists "tenant_id" ON galerie_dir (tenant_id);
select app_private.drop_policies('public.galerie_dir');
create policy admin_all on galerie_dir to administrator using (true) with check (true);
create policy all_view on galerie_dir for select using (true);
create policy my_tenant on galerie_dir as restrictive using (tenant_id = current_tenant_id()) with check (tenant_id = current_tenant_id());

alter table galerie_foto add column if not exists id bigint generated always as (gf_id) stored;
alter table galerie_foto add column if not exists tenant_id bigint not null default current_tenant_id();
CREATE INDEX if not exists "tenant_id" ON galerie_foto (tenant_id);
select app_private.drop_policies('public.galerie_foto');
create policy admin_all on galerie_foto to administrator using (true) with check (true);
create policy all_view on galerie_foto for select using (true);
create policy my_tenant on galerie_foto as restrictive using (tenant_id = current_tenant_id()) with check (tenant_id = current_tenant_id());

alter table event add column if not exists tenant_id bigint not null default current_tenant_id();
CREATE INDEX if not exists "tenant_id" ON event (tenant_id);
select app_private.drop_policies('public.event');
create policy manage_all on event to administrator using (true) with check (true);
create policy select_member on event for select to member using (true);
create policy select_public on event for select to anonymous using (is_public = true);
create policy my_tenant on event as restrictive using (tenant_id = current_tenant_id()) with check (tenant_id = current_tenant_id());

alter table form_responses add column if not exists tenant_id bigint not null default current_tenant_id();
CREATE INDEX if not exists "tenant_id" ON form_responses (tenant_id);
grant all on table form_responses to anonymous;
revoke all on form_responses from member;
alter table form_responses enable row level security;
select app_private.drop_policies('public.form_responses');
create policy admin_all on form_responses to administrator using (true) with check (true);
create policy my_tenant on form_responses as restrictive using (tenant_id = current_tenant_id()) with check (tenant_id = current_tenant_id());

alter table location drop column if exists tenant;
revoke all on location from administrator;
revoke all on location from member;
grant all on location to anonymous;
alter table location enable row level security;
select app_private.drop_policies('public.location');
create policy admin_all on location to administrator using (true) with check (true);
create policy public_view on location for select to anonymous;

revoke all on location_attachment from administrator;
revoke all on location_attachment from member;
grant all on location_attachment to anonymous;
alter table location_attachment enable row level security;
select app_private.drop_policies('public.location_attachment');
create policy admin_all on location_attachment to administrator using (true) with check (true);
create policy public_view on location_attachment for select to anonymous;

alter table nabidka add column if not exists id bigint generated always as (n_id) stored;
alter table nabidka add column if not exists tenant_id bigint not null default current_tenant_id();
CREATE INDEX if not exists "tenant_id" ON nabidka (tenant_id);
revoke all on nabidka from member;
grant all on nabidka to anonymous;
select app_private.drop_policies('public.nabidka');
alter table nabidka enable row level security;
create policy admin_all on nabidka to administrator using (true) with check (true);
create policy member_view on nabidka for select to member;
create policy my_tenant on nabidka as restrictive using (tenant_id = current_tenant_id()) with check (tenant_id = current_tenant_id());

alter table nabidka_item add column if not exists id bigint generated always as (ni_id) stored;
alter table nabidka_item add column if not exists tenant_id bigint not null default current_tenant_id();
CREATE INDEX if not exists "tenant_id" ON nabidka_item (tenant_id);
revoke all on nabidka_item from member;
grant all on nabidka_item to anonymous;
select app_private.drop_policies('public.nabidka_item');
alter table nabidka_item enable row level security;
create policy admin_all on nabidka_item to administrator using (true) with check (true);
create policy member_view on nabidka_item for select to member;
create policy manage_own on nabidka_item for all to member
  using (ni_partner in (select current_couple_ids()))
  with check (ni_partner in (select current_couple_ids()));
create policy my_tenant on nabidka_item as restrictive using (tenant_id = current_tenant_id()) with check (tenant_id = current_tenant_id());

alter table pary add column if not exists id bigint generated always as (p_id) stored;

alter table pary_navrh add column if not exists id bigint generated always as (pn_id) stored;
revoke all on pary_navrh from member;
grant all on pary_navrh to anonymous;
select app_private.drop_policies('public.pary_navrh');
alter table pary_navrh enable row level security;
create policy admin_all on pary_navrh to administrator using (true) with check (true);
create policy manage_own on pary_navrh for all
  using (pn_navrhl = current_user_id() or pn_partner = current_user_id() or pn_partnerka = current_user_id())
  with check (pn_navrhl = current_user_id() and (pn_partner = current_user_id() or pn_partnerka = current_user_id()));

alter table platby_group add column if not exists id bigint generated always as (pg_id) stored;
alter table platby_group add column if not exists tenant_id bigint not null default current_tenant_id();
CREATE INDEX if not exists "tenant_id" ON platby_group (tenant_id);
revoke all on platby_group from member;
grant all on platby_group to anonymous;
select app_private.drop_policies('public.platby_group');
alter table platby_group enable row level security;
create policy admin_all on platby_group to administrator using (true) with check (true);
create policy member_view on platby_group for select to member ;
create policy my_tenant on platby_group as restrictive using (tenant_id = current_tenant_id()) with check (tenant_id = current_tenant_id());

alter table platby_category add column if not exists id bigint generated always as (pc_id) stored;
alter table platby_category add column if not exists tenant_id bigint not null default current_tenant_id();
CREATE INDEX if not exists "tenant_id" ON platby_category (tenant_id);
revoke all on platby_category from member;
grant all on platby_category to anonymous;
select app_private.drop_policies('public.platby_category');
alter table platby_category enable row level security;
create policy admin_all on platby_category to administrator using (true) with check (true);
create policy member_view on platby_category for select to member ;
create policy my_tenant on platby_category as restrictive using (tenant_id = current_tenant_id()) with check (tenant_id = current_tenant_id());

alter table platby_item add column if not exists id bigint generated always as (pi_id) stored;
alter table platby_item add column if not exists tenant_id bigint not null default current_tenant_id();
CREATE INDEX if not exists "tenant_id" ON platby_item (tenant_id);
revoke all on platby_item from member;
grant all on platby_item to anonymous;
select app_private.drop_policies('public.platby_item');
alter table platby_item enable row level security;
create policy admin_all on platby_item to administrator using (true) with check (true);
create policy member_view on platby_item for select to member using (pi_id_user = current_user_id());
create policy my_tenant on platby_item as restrictive using (tenant_id = current_tenant_id()) with check (tenant_id = current_tenant_id());

alter table platby_raw add column if not exists id bigint generated always as (pr_id) stored;
alter table platby_raw add column if not exists tenant_id bigint not null default current_tenant_id();
CREATE INDEX if not exists "tenant_id" ON platby_raw (tenant_id);
revoke all on platby_raw from member;
grant all on platby_raw to anonymous;
select app_private.drop_policies('public.platby_raw');
alter table platby_raw enable row level security;
create policy admin_all on platby_raw to administrator using (true) with check (true);
create policy member_view on platby_raw for select to member using (exists (select from platby_item where pi_id_raw = pr_id and pi_id_user = current_user_id()));
create policy my_tenant on platby_raw as restrictive using (tenant_id = current_tenant_id()) with check (tenant_id = current_tenant_id());

alter table platby_category_group add column if not exists id bigint generated always as (pcg_id) stored;
alter table platby_category_group add column if not exists tenant_id bigint not null default current_tenant_id();
CREATE INDEX if not exists "tenant_id" ON platby_category_group (tenant_id);
revoke all on platby_category_group from member;
grant all on platby_category_group to anonymous;
select app_private.drop_policies('public.platby_category_group');
alter table platby_category_group enable row level security;
create policy admin_all on platby_category_group to administrator using (true) with check (true);
create policy member_view on platby_category_group for select to member ;
create policy my_tenant on platby_category_group as restrictive using (tenant_id = current_tenant_id()) with check (tenant_id = current_tenant_id());

alter table platby_group_skupina add column if not exists id bigint generated always as (pgs_id) stored;
alter table platby_group_skupina add column if not exists tenant_id bigint not null default current_tenant_id();
CREATE INDEX if not exists "tenant_id" ON platby_group_skupina (tenant_id);
revoke all on platby_group_skupina from member;
grant all on platby_group_skupina to anonymous;
select app_private.drop_policies('public.platby_group_skupina');
alter table platby_group_skupina enable row level security;
create policy admin_all on platby_group_skupina to administrator using (true) with check (true);
create policy member_view on platby_group_skupina for select to member ;
create policy my_tenant on platby_group_skupina as restrictive using (tenant_id = current_tenant_id()) with check (tenant_id = current_tenant_id());

revoke all on room from administrator;
revoke all on room from member;
grant all on room to anonymous;
alter table room enable row level security;
select app_private.drop_policies('public.room');
create policy admin_all on room to administrator using (true) with check (true);
create policy public_view on room for select to anonymous;

revoke all on room_attachment from administrator;
revoke all on room_attachment from member;
grant all on room_attachment to anonymous;
alter table room_attachment enable row level security;
select app_private.drop_policies('public.room_attachment');
create policy admin_all on room_attachment to administrator using (true) with check (true);
create policy public_view on room_attachment for select to anonymous;

alter table rozpis add column if not exists id bigint generated always as (r_id) stored;
alter table rozpis add column if not exists tenant_id bigint not null default current_tenant_id();
CREATE INDEX if not exists "tenant_id" ON rozpis (tenant_id);
revoke all on rozpis from member;
grant all on rozpis to anonymous;
select app_private.drop_policies('public.rozpis');
alter table rozpis enable row level security;
create policy admin_all on rozpis to administrator using (true) with check (true);
create policy member_view on rozpis for select to member;
create policy my_tenant on rozpis as restrictive using (tenant_id = current_tenant_id()) with check (tenant_id = current_tenant_id());

alter table rozpis_item add column if not exists id bigint generated always as (ri_id) stored;
alter table rozpis_item add column if not exists tenant_id bigint not null default current_tenant_id();
CREATE INDEX if not exists "tenant_id" ON rozpis_item (tenant_id);
revoke all on rozpis_item from member;
grant all on rozpis_item to anonymous;
select app_private.drop_policies('public.rozpis_item');
create policy admin_all on rozpis_item to administrator using (true) with check (true);
create policy member_view on rozpis_item for select to member;
create policy manage_own on rozpis_item for all to member
  using (ri_partner in (select current_couple_ids()))
  with check (ri_partner in (select current_couple_ids()));
create policy my_tenant on rozpis_item as restrictive using (tenant_id = current_tenant_id()) with check (tenant_id = current_tenant_id());

alter table skupiny add column if not exists id bigint generated always as (s_id) stored;
alter table skupiny add column if not exists tenant_id bigint not null default current_tenant_id();
CREATE INDEX if not exists "tenant_id" ON skupiny (tenant_id);
select app_private.drop_policies('public.skupiny');
create policy admin_all on skupiny to administrator using (true) with check (true);
create policy all_view on skupiny for select using (true);
create policy my_tenant on skupiny as restrictive using (tenant_id = current_tenant_id()) with check (tenant_id = current_tenant_id());


revoke all on tenant from administrator;
revoke all on tenant from member;
grant all on tenant to anonymous;
alter table tenant enable row level security;
select app_private.drop_policies('public.tenant');
create policy admin_all on tenant to administrator using (true) with check (true);
create policy public_view on tenant for select to anonymous;
create policy my_tenant on tenant as restrictive using (id = current_tenant_id()) with check (id = current_tenant_id());

revoke all on tenant_attachment from administrator;
revoke all on tenant_attachment from member;
grant all on tenant_attachment to anonymous;
alter table tenant_attachment enable row level security;
select app_private.drop_policies('public.tenant_attachment');
create policy admin_all on tenant_attachment to administrator using (true) with check (true);
create policy public_view on tenant_attachment for select to anonymous;
create policy my_tenant on tenant_attachment as restrictive using (tenant_id = current_tenant_id()) with check (tenant_id = current_tenant_id());

DROP TABLE IF EXISTS tenant_location CASCADE;
CREATE TABLE tenant_location (
       tenant_id bigint references tenant,
       location_id bigint references location,
       primary key (tenant_id, location_id)
);
grant all on tenant_location to anonymous;
alter table tenant_location enable row level security;
select app_private.drop_policies('public.tenant_location');
create policy admin_all on tenant_location to administrator using (true) with check (true);
create policy public_view on tenant_location for select to anonymous;
CREATE INDEX ON "public"."tenant_location"("location_id");

revoke all on tenant_person from administrator;
revoke all on tenant_person from member;
grant all on tenant_person to anonymous;
alter table tenant_person enable row level security;
select app_private.drop_policies('public.tenant_person');
create policy admin_all on tenant_person to administrator using (true) with check (true);
create policy public_view on tenant_person for select to anonymous;
create policy my_tenant on tenant_person as restrictive using (tenant_id = current_tenant_id()) with check (tenant_id = current_tenant_id());

alter table upozorneni add column if not exists id bigint generated always as (up_id) stored;
alter table upozorneni add column if not exists tenant_id bigint not null default current_tenant_id();
CREATE INDEX if not exists "tenant_id" ON upozorneni (tenant_id);
revoke all on upozorneni from member;
grant all on upozorneni to anonymous;
alter table upozorneni enable row level security;
select app_private.drop_policies('public.upozorneni');
create policy admin_all on upozorneni to administrator using (true) with check (true);
create policy member_view on upozorneni for select to member;
create policy my_tenant on upozorneni as restrictive using (tenant_id = current_tenant_id()) with check (tenant_id = current_tenant_id());

alter table upozorneni_skupiny add column if not exists id bigint generated always as (ups_id) stored;
alter table upozorneni_skupiny add column if not exists tenant_id bigint not null default current_tenant_id();
CREATE INDEX if not exists "tenant_id" ON upozorneni_skupiny (tenant_id);
revoke all on upozorneni from member;
revoke all on upozorneni_skupiny from member;
grant all on upozorneni_skupiny to anonymous;
alter table upozorneni_skupiny enable row level security;
select app_private.drop_policies('public.upozorneni_skupiny');
create policy admin_all on upozorneni_skupiny to administrator using (true) with check (true);
create policy member_view on upozorneni_skupiny for select to member;
create policy my_tenant on upozorneni_skupiny as restrictive using (tenant_id = current_tenant_id()) with check (tenant_id = current_tenant_id());

alter table users add column if not exists id bigint generated always as (u_id) stored;
alter table users add column if not exists tenant_id bigint not null default current_tenant_id();
CREATE INDEX if not exists "tenant_id" ON users (tenant_id);
revoke all on users from member;
select app_private.drop_policies('public.users');
create policy admin_all on users to administrator using (true) with check (true);
create policy all_view on users for select to member using (true);
create policy manage_own on users for all
  using (u_id = current_user_id()) with check (u_id = current_user_id());
create policy register_anonymous on users for insert with check (u_confirmed=false);
create policy my_tenant on users as restrictive using (tenant_id = current_tenant_id()) with check (tenant_id = current_tenant_id());

CREATE INDEX if not exists "object_name" ON "public"."location_attachment"("object_name");
CREATE INDEX if not exists "object_name" ON "public"."room_attachment"("object_name");
CREATE INDEX if not exists "object_name" ON "public"."tenant_attachment"("object_name");
CREATE INDEX if not exists "uploaded_by" ON "public"."attachment"("uploaded_by");
CREATE INDEX if not exists "event_id" ON "public"."attendee_external"("event_id");
CREATE INDEX if not exists "managed_by" ON "public"."attendee_external"("managed_by");
CREATE INDEX if not exists "confirmed_by" ON "public"."attendee_external"("confirmed_by");
CREATE INDEX if not exists "cohort_group" ON "public"."skupiny"("cohort_group");
CREATE INDEX if not exists "tenant_id" ON "public"."cohort_group"("tenant_id");
CREATE INDEX if not exists "location" ON "public"."room"("location");
CREATE INDEX if not exists "person_id" ON "public"."tenant_person"("person_id");
CREATE INDEX if not exists "ss_user" ON "public"."session"("ss_user");

create index if not exists "d_kategorie" on public.dokumenty (d_kategorie);
create index if not exists "d_timestamp" on public.dokumenty (d_timestamp);
create index if not exists "ri_od" on public.rozpis_item (ri_od);
create index if not exists "n_od" on public.nabidka (n_od);
create index if not exists "updated_at" on public.form_responses (updated_at);
create index if not exists "type" on public.form_responses (type);
create index if not exists "u_system" on public.users (u_system);
create index if not exists "u_confirmed" on public.users (u_confirmed);
create index if not exists "u_ban" on public.users (u_ban);
create index if not exists "u_prijmeni" on public.users (u_prijmeni);
create index if not exists "u_jmeno" on public.users (u_jmeno);
create index if not exists "is_public" on public.cohort_group (is_public);
create index if not exists "ordering" on public.cohort_group (ordering);
create index if not exists "s_visible" on public.skupiny (s_visible);
create index if not exists "ordering" on public.skupiny (ordering);
create index if not exists "is_visible" on public.event (is_visible);
create index if not exists "since" on public.event (since);
create index if not exists "r_datum" on public.rozpis (r_datum);
