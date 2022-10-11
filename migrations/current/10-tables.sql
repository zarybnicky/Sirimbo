drop view if exists nabidka_admin;
drop view if exists rozpis_admin;
drop view if exists aktuality_admin;
drop table if exists aktuality_foto;

-- ************** akce *************
select app_private.drop_policies('public.akce');
alter table akce enable row level security;
create policy manage_all on akce to administrator using (true) with check (true);
create policy select_all on akce for select using (true);
grant all on akce to anonymous;

-- ************** akce_item *************
select app_private.drop_policies('public.akce_item');
alter table akce_item enable row level security;
create policy admin_all on akce_item to administrator using (true) with check (true);
create policy all_view on akce_item for select using (true);
create policy manage_own on akce_item for all to member
  using (ai_user = current_user_id())
  with check (ai_user = current_user_id());
grant all on akce_item to anonymous;

-- ************** aktuality **************
select app_private.drop_policies('public.aktuality');
alter table aktuality enable row level security;
create policy admin_all on aktuality to administrator using (true) with check (true);
create policy all_view on aktuality for select using (true);
grant all on aktuality to anonymous;

-- ************** dokumenty **************
select app_private.drop_policies('public.dokumenty');
alter table dokumenty enable row level security;
create policy admin_all on dokumenty to administrator using (true) with check (true);
create policy all_view on dokumenty for select using (true);
grant all on dokumenty to anonymous;

-- ************** galerie_dir **************
select app_private.drop_policies('public.galerie_dir');
alter table galerie_dir enable row level security;
create policy admin_all on galerie_dir to administrator using (true) with check (true);
create policy all_view on galerie_dir for select using (true);
grant all on galerie_dir to anonymous;

-- ************** galerie_foto **************
select app_private.drop_policies('public.galerie_foto');
alter table galerie_foto enable row level security;
create policy admin_all on galerie_foto to administrator using (true) with check (true);
create policy all_view on galerie_foto for select using (true);
grant all on galerie_foto to anonymous;

-- ************** nabidka **************
select app_private.drop_policies('public.nabidka');
alter table nabidka enable row level security;
create policy admin_all on nabidka to administrator using (true) with check (true);
create policy all_view on nabidka for select using (true);
grant all on nabidka to member;

-- ************** nabidka_item *************
select app_private.drop_policies('public.nabidka_item');
alter table nabidka_item enable row level security;
create policy admin_all on nabidka_item to administrator using (true) with check (true);
create policy all_view on nabidka_item for select using (true);
create policy manage_own on nabidka_item for all to member
  using (ni_partner in (select current_couple_ids()))
  with check (ni_partner in (select current_couple_ids()));
grant all on nabidka_item to member;

-- ************** page **************
select app_private.drop_policies('public.page');
alter table page enable row level security;
create policy admin_all on page to administrator using (true) with check (true);
create policy all_view on page for select using (true);
grant usage, select on sequence page_id_seq to administrator;
grant all on page to anonymous;

-- ************** page_revision **************
select app_private.drop_policies('public.page_revision');
alter table page_revision enable row level security;
create policy all_view on page_revision for select using (true);
grant all on page_revision to anonymous;

-- ************** parameters **************
select app_private.drop_policies('public.parameters');
alter table parameters enable row level security;
create policy admin_all on parameters to administrator using (true) with check (true);
create policy all_view on parameters for select using (true);
grant all on parameters to anonymous;

-- ************** pary **************
select app_private.drop_policies('public.pary');
alter table pary enable row level security;
create policy admin_all on pary to administrator using (true) with check (true);
create policy all_view on pary for select using (true);
grant all on pary to anonymous;

-- ************** pary_navrh **************
select app_private.drop_policies('public.pary_navrh');
create policy manage_own on pary_navrh for all
  using (pn_navrhl = current_user_id() or pn_partner = current_user_id() or pn_partnerka = current_user_id())
  with check (pn_navrhl = current_user_id() and (pn_partner = current_user_id() or pn_partnerka = current_user_id()));
grant all on pary_navrh to member;

-- ************** permissions **************
select app_private.drop_policies('public.permissions');
alter table permissions enable row level security;
create policy admin_all on permissions to administrator using (true) with check (true);
create policy all_view on permissions for select using (true);
grant all on permissions to anonymous;

-- ************** platby_category **************
select app_private.drop_policies('public.platby_category');
alter table platby_category enable row level security;
create policy admin_all on platby_category to administrator using (true) with check (true);
create policy all_view on platby_category for select using (true);
grant all on platby_category to member;

-- ************** platby_category_group **************
select app_private.drop_policies('public.platby_category_group');
alter table platby_category_group enable row level security;
create policy admin_all on platby_category_group to administrator using (true) with check (true);
create policy all_view on platby_category_group for select using (true);
grant all on platby_category_group to member;

-- ************** platby_group **************
select app_private.drop_policies('public.platby_group');
alter table platby_group enable row level security;
create policy admin_all on platby_group to administrator using (true) with check (true);
create policy all_view on platby_group for select using (true);
grant all on platby_group to member;

-- ************** platby_group_skupina **************
select app_private.drop_policies('public.platby_group_skupina');
alter table platby_group_skupina enable row level security;
create policy admin_all on platby_group_skupina to administrator using (true) with check (true);
create policy all_view on platby_group_skupina for select using (true);
grant all on platby_group_skupina to member;

-- ************** platby_item **************
select app_private.drop_policies('public.platby_item');
alter table platby_item enable row level security;
create policy admin_all on platby_item to administrator using (true) with check (true);
create policy all_view on platby_item for select using (true);
grant all on platby_item to member;

-- ************** platby_raw **************
select app_private.drop_policies('public.platby_raw');
alter table platby_raw enable row level security;
create policy admin_all on platby_raw to administrator using (true) with check (true);
create policy all_view on platby_raw for select using (true);
grant all on platby_raw to member;

-- ************** rozpis **************
select app_private.drop_policies('public.rozpis');
alter table rozpis enable row level security;
create policy admin_all on rozpis to administrator using (true) with check (true);
create policy all_view on rozpis for select using (true);
grant all on rozpis to member;

-- ************** rozpis_item **************
select app_private.drop_policies('public.rozpis_item');
alter table rozpis_item enable row level security;
create policy admin_all on rozpis_item to administrator using (true) with check (true);
create policy all_view on rozpis_item for select using (true);
create policy manage_own on rozpis_item for all to member
  using (ri_partner in (select current_couple_ids()))
  with check (ri_partner in (select current_couple_ids()));
grant all on rozpis_item to member;

-- ************** session **************
ALTER TABLE session DROP COLUMN IF EXISTS ss_user CASCADE;
ALTER TABLE session ADD COLUMN ss_user bigint null default null;
ALTER TABLE ONLY public.session
  ADD CONSTRAINT session_ss_user_fkey FOREIGN KEY (ss_user) REFERENCES users(u_id) ON DELETE CASCADE;

select app_private.drop_policies('public.session');
alter table session enable row level security;
create policy admin_all on session to administrator using (true) with check (true);
create policy manage_own on session for all
  using (ss_user = current_user_id())
  with check (ss_user = current_user_id());

-- ************** skupiny **************
do $$ begin
  if not exists (select 1
     from information_schema.columns
     where table_schema = 'public' and table_name = 'skupiny' and column_name = 's_location'
  ) then
    alter table public.skupiny add column s_location text not null default '';
  end if;
end $$;

do $$ begin
  if not exists (select 1
     from information_schema.columns
     where table_schema = 'public' and table_name = 'skupiny' and column_name = 's_visible'
  ) then
    alter table public.skupiny add column s_visible boolean not null default true;
  end if;
end $$;

select app_private.drop_policies('public.skupiny');
alter table skupiny enable row level security;
create policy admin_all on skupiny to administrator using (true) with check (true);
create policy all_view on skupiny for select using (true);
grant all on skupiny to anonymous;

-- ************** upozorneni **************
select app_private.drop_policies('public.upozorneni');
alter table upozorneni enable row level security;
create policy admin_all on upozorneni to administrator using (true) with check (true);
create policy all_view on upozorneni for select using (true);
grant all on upozorneni to member;

-- ************** upozorneni_skupiny **************
select app_private.drop_policies('public.upozorneni_skupiny');
alter table upozorneni_skupiny enable row level security;
create policy admin_all on upozorneni_skupiny to administrator using (true) with check (true);
create policy all_view on upozorneni_skupiny for select using (true);
grant all on upozorneni_skupiny to member;

-- ************** users **************
select app_private.drop_policies('public.users');
alter table users enable row level security;
create policy admin_all on users to administrator using (true) with check (true);
create policy all_view on users for select using (true);
create policy manage_own on users for all
  using (u_id = current_user_id()) with check (u_id = current_user_id());
create policy register_anonymous on users for insert with check (u_confirmed=false);

alter table users alter column u_confirmed set default false;
alter table users alter column u_group set default 0;
alter table users alter column u_teacher set default false;
alter table users alter column u_gdpr_signed_at set default CURRENT_TIMESTAMP;

grant all on users to member;
grant insert (u_id, u_login, u_pass, u_jmeno, u_prijmeni, u_pohlavi, u_email,
u_telefon, u_narozeni, u_rodne_cislo, u_poznamky, u_dancer, u_street,
u_conscription_number, u_orientation_number, u_district, u_city,
u_postal_code, u_nationality) on users to anonymous;

-- ************** users_skupiny **************
select app_private.drop_policies('public.users_skupiny');
alter table users_skupiny enable row level security;
create policy admin_all on users_skupiny to administrator using (true) with check (true);
create policy all_view on users_skupiny for select using (true);
grant all on users_skupiny to member;

-- ************** video **************
select app_private.drop_policies('public.video');
alter table video enable row level security;
create policy admin_all on video to administrator using (true) with check (true);
create policy all_view on video for select using (true);
grant all on video to anonymous;

-- ************** video_list **************
select app_private.drop_policies('public.video_list');
alter table video_list enable row level security;
create policy admin_all on video_list to administrator using (true) with check (true);
create policy all_view on video_list for select using (true);
grant all on video_list to anonymous;

-- ************** video_source **************
select app_private.drop_policies('public.video_source');
alter table video_source enable row level security;
create policy admin_all on video_source to administrator using (true) with check (true);
create policy all_view on video_source for select using (true);
grant all on video_source to anonymous;

grant usage, select on all sequences in schema public to anonymous;
grant execute on all functions in schema public to anonymous;


-- alter table app_public.posts enable row level security;
-- grant select on app_public.user_feed_posts to :DATABASE_VISITOR;
-- grant
--   select,
--   insert (headline, body, topic),
--   update (headline, body, topic),
--   delete
-- on app_public.posts to :DATABASE_VISITOR;
-- create policy select_own on app_public.user_feed_posts for select using (user_id = app_public.current_user_id());
-- create policy select_all on app_public.posts for select using (true);
-- create policy manage_own on app_public.posts for all using (author_id = app_public.current_user_id());
-- create policy manage_as_admin on app_public.posts for all using (exists (select 1 from app_public.users where is_admin is true and id = app_public.current_user_id()));
