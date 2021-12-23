drop table if exists aktuality_foto;

create or replace function public.login(login varchar, passwd varchar) returns varchar as $$
declare
  v_user users;
  v_sid varchar;
  v_salt varchar;
begin
  if username like '%@%' then
    select users.* into v_user from users where u_email = login limit 1;
  else
    select users.* into v_user from users where u_login = login limit 1;
  end if;

  if v_user is null then
    raise exception 'Account not found' using errcode = 'NOACCT';
  end if;

  select encode(digest('######TK.-.OLYMP######', 'md5'), 'hex') into v_salt;
  if v_user.u_pass != encode(digest(v_salt || passwd || v_salt, 'sha1'), 'hex') then
    raise exception 'Invalid password' using errcode = 'PASSWD';
  end if;

  if v_user.u_ban then
    raise exception 'This account is disabled' using errcode = 'BAN';
  end if;
  if not v_user.u_confirmed then
    raise exception 'This account is not yet confirmed' using errcode = 'CONFIRM';
  end if;

  select gen_random_uuid() into v_sid;
  insert into session
    (ss_id, ss_user, ss_data, ss_lifetime)
    VALUES (v_sid, v_user.u_id, json_build_object('id', v_user.u_id), 86400);

  return v_sid;
end;
$$ language plpgsql strict volatile security definer;
select * from plpgsql_check_function('public.login');


create or replace function public.logout() returns void as $$
declare
  v_user users;
  v_sid varchar;
  v_salt varchar;
begin
  delete from session where ss_id=current_session_id();
end;
$$ language plpgsql strict volatile security definer;
select * from plpgsql_check_function('public.logout');


drop function current_user_id() cascade;
create or replace function current_user_id() returns bigint as $$
  SELECT current_setting('jwt.claims.user_id', true)::bigint;
$$ language sql stable;

create or replace function current_session_id() returns text as $$
  select current_setting('jwt.claims.session_id', true);
$$ language sql stable;

create or replace function current_couple_ids() returns setof bigint AS $$
  select distinct p_id_partner
  from public.pary
  where p_id_partner = current_user_id() and p_archiv = false
  UNION
  select distinct p_id_partnerka
  from public.pary
  where p_id_partnerka = current_user_id() and p_archiv = false;
$$ language sql stable;

create or replace function app_private.drop_policies(tbl text) returns void as $$
declare
   rec record;
begin
   for rec in (
     select policyname from pg_policies
     where schemaname = split_part(tbl, '.', 1) and tablename = split_part(tbl, '.', 2)
   ) loop
     execute 'drop policy "' || rec.policyname || '" on ' || tbl;
   end loop;
end;
$$ language plpgsql volatile;
select * from plpgsql_check_function('app_private.drop_policies');

-- ************** akce *************
select app_private.drop_policies('public.akce');
alter table akce enable row level security;
create policy admin_all on akce to administrator using (true) with check (true);
create policy all_view on akce for select using (true);

-- ************** akce_item *************
select app_private.drop_policies('public.akce_item');
alter table akce_item enable row level security;
create policy admin_all on akce_item to administrator using (true) with check (true);
create policy all_view on akce_item for select using (true);
create policy manage_own on akce_item for all to member
  using (ai_user = current_user_id())
  with check (ai_user = current_user_id());

-- ************** aktuality **************
select app_private.drop_policies('public.aktuality');
alter table aktuality enable row level security;
create policy admin_all on aktuality to administrator using (true) with check (true);
create policy all_view on aktuality for select using (true);

-- ************** dokumenty **************
select app_private.drop_policies('public.dokumenty');
alter table dokumenty enable row level security;
create policy admin_all on dokumenty to administrator using (true) with check (true);
create policy all_view on dokumenty for select using (true);

-- ************** galerie_dir **************
select app_private.drop_policies('public.galerie_dir');
alter table galerie_dir enable row level security;
create policy admin_all on galerie_dir to administrator using (true) with check (true);
create policy all_view on galerie_dir for select using (true);

-- ************** galerie_foto **************
select app_private.drop_policies('public.galerie_foto');
alter table galerie_foto enable row level security;
create policy admin_all on galerie_foto to administrator using (true) with check (true);
create policy all_view on galerie_foto for select using (true);

-- ************** nabidka **************
select app_private.drop_policies('public.nabidka');
alter table nabidka enable row level security;
create policy admin_all on nabidka to administrator using (true) with check (true);
create policy all_view on nabidka for select using (true);

-- ************** nabidka_item *************
select app_private.drop_policies('public.nabidka_item');
alter table nabidka_item enable row level security;
create policy admin_all on nabidka_item to administrator using (true) with check (true);
create policy all_view on nabidka_item for select using (true);
create policy manage_own on nabidka_item for all to member
  using (ni_partner in (select * from current_couple_ids()))
  with check (ni_partner in (select * from current_couple_ids()));

-- ************** page **************
select app_private.drop_policies('public.page');
alter table page enable row level security;
create policy admin_all on page to administrator using (true) with check (true);
create policy all_view on page for select using (true);

-- ************** page_revision **************
select app_private.drop_policies('public.page_revision');
alter table page_revision enable row level security;
create policy all_view on page_revision for select using (true);

-- ************** parameters **************
select app_private.drop_policies('public.parameters');
alter table parameters enable row level security;
create policy admin_all on parameters to administrator using (true) with check (true);
create policy all_view on parameters for select using (true);

-- ************** pary **************
select app_private.drop_policies('public.pary');
alter table pary enable row level security;
create policy admin_all on pary to administrator using (true) with check (true);
create policy all_view on pary for select using (true);

-- ************** pary_navrh **************
select app_private.drop_policies('public.pary_navrh');
create policy manage_own on pary_navrh for all
  using (pn_navrhl = current_user_id() or pn_partner = current_user_id() or pn_partnerka = current_user_id())
  with check (pn_navrhl = current_user_id() and (pn_partner = current_user_id() or pn_partnerka = current_user_id()));

-- ************** permissions **************
select app_private.drop_policies('public.permissions');
alter table permissions enable row level security;
create policy admin_all on permissions to administrator using (true) with check (true);
create policy all_view on permissions for select using (true);

-- ************** platby_category **************
select app_private.drop_policies('public.platby_category');
alter table platby_category enable row level security;
create policy admin_all on platby_category to administrator using (true) with check (true);
create policy all_view on platby_category for select using (true);

-- ************** platby_category_group **************
select app_private.drop_policies('public.platby_category_group');
alter table platby_category_group enable row level security;
create policy admin_all on platby_category_group to administrator using (true) with check (true);
create policy all_view on platby_category_group for select using (true);

-- ************** platby_group **************
select app_private.drop_policies('public.platby_group');
alter table platby_group enable row level security;
create policy admin_all on platby_group to administrator using (true) with check (true);
create policy all_view on platby_group for select using (true);

-- ************** platby_group_skupina **************
select app_private.drop_policies('public.platby_group_skupina');
alter table platby_group_skupina enable row level security;
create policy admin_all on platby_group_skupina to administrator using (true) with check (true);
create policy all_view on platby_group_skupina for select using (true);

-- ************** platby_item **************
select app_private.drop_policies('public.platby_item');
alter table platby_item enable row level security;
create policy admin_all on platby_item to administrator using (true) with check (true);
create policy all_view on platby_item for select using (true);

-- ************** platby_raw **************
select app_private.drop_policies('public.platby_raw');
alter table platby_raw enable row level security;
create policy admin_all on platby_raw to administrator using (true) with check (true);
create policy all_view on platby_raw for select using (true);

-- ************** rozpis **************
select app_private.drop_policies('public.rozpis');
alter table rozpis enable row level security;
create policy admin_all on rozpis to administrator using (true) with check (true);
create policy all_view on rozpis for select using (true);

-- ************** rozpis_item **************
select app_private.drop_policies('public.rozpis_item');
alter table rozpis_item enable row level security;
create policy admin_all on rozpis_item to administrator using (true) with check (true);
create policy all_view on rozpis_item for select using (true);
create policy manage_own on rozpis_item for all to member
  using (ri_partner in (select * from current_couple_ids()))
  with check (ri_partner in (select * from current_couple_ids()));

-- ************** session **************
select app_private.drop_policies('public.rozpis_item');
alter table rozpis_item enable row level security;
create policy admin_all on rozpis_item to administrator using (true) with check (true);
create policy all_view on rozpis_item for select using (true);
create policy manage_own on rozpis_item for all to member
  using (ri_partner in (select * from current_couple_ids()))
  with check (ri_partner in (select * from current_couple_ids()));

-- ************** skupiny **************
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

-- ************** upozorneni **************
select app_private.drop_policies('public.upozorneni');
alter table upozorneni enable row level security;
create policy admin_all on upozorneni to administrator using (true) with check (true);
create policy all_view on upozorneni for select using (true);

-- ************** upozorneni_skupiny **************
select app_private.drop_policies('public.upozorneni_skupiny');
alter table upozorneni_skupiny enable row level security;
create policy admin_all on upozorneni_skupiny to administrator using (true) with check (true);
create policy all_view on upozorneni_skupiny for select using (true);

-- ************** users **************
select app_private.drop_policies('public.users');
alter table users enable row level security;
create policy admin_all on users to administrator using (true) with check (true);
create policy all_view on users for select using (true);
create policy manage_own on users for all
  using (u_id = current_user_id()) with check (u_id = current_user_id());

-- ************** users_skupiny **************
select app_private.drop_policies('public.users_skupiny');
alter table users_skupiny enable row level security;
create policy admin_all on users_skupiny to administrator using (true) with check (true);
create policy all_view on users_skupiny for select using (true);

-- ************** video **************
select app_private.drop_policies('public.video');
alter table video enable row level security;
create policy admin_all on video to administrator using (true) with check (true);
create policy all_view on video for select using (true);

-- ************** video_list **************
select app_private.drop_policies('public.video_list');
alter table video_list enable row level security;
create policy admin_all on video_list to administrator using (true) with check (true);
create policy all_view on video_list for select using (true);

-- ************** video_source **************
select app_private.drop_policies('public.video_source');
alter table video_source enable row level security;
create policy admin_all on video_source to administrator using (true) with check (true);
create policy all_view on video_source for select using (true);


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
