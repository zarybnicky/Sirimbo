--! Previous: -
--! Hash: sha1:c92280a1a093afdd296ca43acd91153fd819a8cb

--! split: 01-utils.sql
create schema if not exists app_private;
grant all on schema app_private to postgres;
grant all on schema app_private to :DATABASE_OWNER;

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

create or replace function app_private.tg__timestamps() returns trigger as $$
begin
  NEW.created_at = (case when TG_OP = 'INSERT' then NOW() else OLD.created_at end);
  NEW.updated_at = (case when TG_OP = 'UPDATE' and OLD.updated_at >= NOW() then OLD.updated_at + interval '1 millisecond' else NOW() end);
  return NEW;
end;
$$ language plpgsql volatile;
comment on function app_private.tg__timestamps() is
  E'This trigger should be called on all tables with created_at, updated_at - it ensures that they cannot be manipulated and that updated_at will always be larger than the previous updated_at.';


create or replace function current_session_id() returns text as $$
  select nullif(current_setting('jwt.claims.user_id', true), '')::integer;
$$ language sql stable;
grant execute on function current_session_id to anonymous;

create or replace function current_user_id() returns bigint as $$
  SELECT current_setting('jwt.claims.user_id', true)::bigint;
$$ language sql stable;
grant execute on function current_user_id to anonymous;

create or replace function current_permissions() returns setof permissions as $$
  SELECT permissions.* from permissions
  inner join users on u_group=pe_id
  where u_id=current_setting('jwt.claims.user_id', true)::bigint;
$$ language sql stable;
grant execute on function current_user_id to anonymous;

create or replace function current_couple_ids() returns setof bigint AS $$
  select distinct p_id
  from public.pary
  where p_id_partner = current_user_id() and p_archiv = false
  UNION
  select distinct p_id
  from public.pary
  where p_id_partnerka = current_user_id() and p_archiv = false;
$$ language sql stable;
grant execute on function current_couple_ids to anonymous;

--! split: 10-tables.sql
drop view if exists nabidka_admin;
drop view if exists rozpis_admin;
drop view if exists aktuality_admin;
drop table if exists aktuality_foto;
drop table if exists users_skupiny;

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

ALTER TABLE ONLY public.akce_item
drop constraint akce_item_ai_id_rodic_fkey,
ADD CONSTRAINT akce_item_ai_id_rodic_fkey FOREIGN KEY (ai_id_rodic) REFERENCES public.akce(a_id) ON UPDATE CASCADE ON DELETE CASCADE;


-- ************** aktuality **************
select app_private.drop_policies('public.aktuality');
alter table aktuality enable row level security;
create policy admin_all on aktuality to administrator using (true) with check (true);
create policy all_view on aktuality for select using (true);
grant all on aktuality to anonymous;

alter table aktuality alter column at_timestamp_add set default now();
alter table aktuality alter column at_kdo drop not null;

CREATE or replace FUNCTION public.on_update_author_aktuality() RETURNS trigger LANGUAGE plpgsql AS $$
BEGIN
   NEW.at_kdo = current_user_id();
   NEW.at_timestamp = now();
   RETURN NEW;
END;
$$;
select * from plpgsql_check_function('public.on_update_author_aktuality', 'aktuality');

drop trigger if exists on_update_author on public.aktuality;
create trigger on_update_author
  before update on public.aktuality
  for each row
  execute procedure public.on_update_author_aktuality();



-- ************** dokumenty **************
select app_private.drop_policies('public.dokumenty');
alter table dokumenty enable row level security;
create policy admin_all on dokumenty to administrator using (true) with check (true);
create policy all_view on dokumenty for select using (true);
grant all on dokumenty to member;

CREATE or replace FUNCTION public.on_delete_file_dokumenty() RETURNS trigger LANGUAGE plpgsql AS $$
BEGIN
    perform graphile_worker.add_job('delete_file', json_build_object('path', OLD.d_path));
    return old;
END;
$$;
select * from plpgsql_check_function('public.on_delete_file_dokumenty', 'dokumenty');

drop trigger if exists on_delete_file_dokumenty on public.dokumenty;
create trigger on_delete_file_dokumenty
  after delete on public.dokumenty
  for each row
  execute procedure public.on_delete_file_dokumenty();


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

alter table nabidka alter column n_max_pocet_hod type smallint;

-- ************** nabidka_item *************
select app_private.drop_policies('public.nabidka_item');
alter table nabidka_item enable row level security;
create policy admin_all on nabidka_item to administrator using (true) with check (true);
create policy all_view on nabidka_item for select using (true);
create policy manage_own on nabidka_item for all to member
  using (ni_partner in (select current_couple_ids()))
  with check (ni_partner in (select current_couple_ids()));
grant all on nabidka_item to member;

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

alter table public.pary
drop constraint pary_p_id_partner_fkey,
add constraint pary_p_id_partner_fkey foreign key (p_id_partner) references public.users(u_id) on update cascade on delete cascade;

comment on table public.pary is E'@foreignKey (p_id_partnerka) references users (u_id)';

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
ALTER TABLE session DROP COLUMN IF EXISTS ss_data CASCADE;
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

alter table upozorneni alter column up_kdo drop not null;

CREATE or replace FUNCTION public.on_update_author_upozorneni() RETURNS trigger LANGUAGE plpgsql AS $$
BEGIN
   NEW.up_kdo = current_user_id();
   NEW.up_timestamp = now();
   RETURN NEW;
END;
$$;
select * from plpgsql_check_function('public.on_update_author_upozorneni', 'upozorneni');

drop trigger if exists on_update_author_upozorneni on public.upozorneni;
create trigger on_update_author_upozorneni
  before insert or update on public.upozorneni
  for each row
  execute procedure public.on_update_author_upozorneni();

-- ************** upozorneni_skupiny **************
select app_private.drop_policies('public.upozorneni_skupiny');
alter table upozorneni_skupiny enable row level security;
create policy admin_all on upozorneni_skupiny to administrator using (true) with check (true);
create policy all_view on upozorneni_skupiny for select using (true);
grant all on upozorneni_skupiny to member;

alter table public.upozorneni_skupiny
drop constraint upozorneni_skupiny_ups_id_rodic_fkey,
ADD CONSTRAINT upozorneni_skupiny_ups_id_rodic_fkey FOREIGN KEY (ups_id_rodic) REFERENCES public.upozorneni(up_id) ON UPDATE CASCADE ON DELETE CASCADE,
drop constraint upozorneni_skupiny_ups_id_skupina_fkey,
ADD CONSTRAINT upozorneni_skupiny_ups_id_skupina_fkey FOREIGN KEY (ups_id_skupina) REFERENCES public.skupiny(s_id) ON UPDATE CASCADE ON DELETE CASCADE;


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

-- ************** video **************
select app_private.drop_policies('public.video');
alter table video enable row level security;
create policy admin_all on video to administrator using (true) with check (true);
create policy all_view on video for select using (true);
grant all on video to anonymous;

alter table video alter column v_created_at set default now();

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

--! split: 20-page.sql
do $$ begin
  if not exists (select 1 from information_schema.tables
    where table_schema = 'public' and table_name = 'page'
  ) then

    create table page (
      id serial primary key,
      url varchar not null unique,
      content json not null,
      created_at timestamptz not null default now(),
      updated_at timestamptz not null default now()
    );
    comment on table page is E'@omit delete';

    CREATE TEMPORARY TABLE IF NOT EXISTS base_revision (
        rev_number INTEGER NOT NULL,
        rev_operation CHAR(1) NOT NULL CHECK (rev_operation IN ('I', 'U', 'D')),
        rev_timestamp TIMESTAMP DEFAULT CURRENT_TIMESTAMP
    );
    create table page_revision (
        like base_revision including constraints,
        like page,
        primary key (rev_number, id)
    );
    comment on table page_revision is E'@omit create,update,delete';

    -- ************** page **************
    perform app_private.drop_policies('public.page');
    alter table page enable row level security;
    create policy admin_all on page to administrator using (true) with check (true);
    create policy all_view on page for select using (true);
    grant usage, select on sequence page_id_seq to administrator;
    grant all on page to anonymous;

    -- ************** page_revision **************
    perform app_private.drop_policies('public.page_revision');
    alter table page_revision enable row level security;
    create policy all_view on page_revision for select using (true);
    grant all on page_revision to anonymous;
  end if;
end $$;

do $$ begin
  if not exists (select 1
     from information_schema.columns
     where table_schema = 'public' and table_name = 'page' and column_name = 'title'
  ) then
    alter table public.page add column title text not null default '';
  end if;
end $$;

do $$ begin
  if not exists (select 1
     from information_schema.columns
     where table_schema = 'public' and table_name = 'page_revision' and column_name = 'title'
  ) then
    alter table public.page_revision add column title text not null default '';
  end if;
end $$;


-- https://dev.to/livioribeiro/use-your-database-part-3---creating-a-revision-system-20j7
CREATE TEMPORARY TABLE IF NOT EXISTS base_revision (
    rev_number INTEGER NOT NULL,
    rev_operation CHAR(1) NOT NULL CHECK (rev_operation IN ('I', 'U', 'D')),
    rev_timestamp TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);

CREATE OR REPLACE FUNCTION app_private.insert_revision() RETURNS TRIGGER AS $$
DECLARE
    _op CHAR(1);
    _record RECORD;
    _rev_number INTEGER;
    _rev_table VARCHAR := TG_TABLE_SCHEMA || '.' || TG_TABLE_NAME || '_revision';
    _where VARCHAR := '';
    _pk VARCHAR;
BEGIN
    IF TG_OP = 'INSERT' THEN
        _op := 'I'; _record := NEW;
    ELSIF TG_OP = 'UPDATE' THEN
        _op := 'U'; _record := NEW;
    ELSE
        _op := 'D'; _record := OLD;
    END IF;
    IF TG_NARGS = 0 THEN
        _where := '_rev_table.id = $1.id';
    ELSE
        _where := format('_rev_table.%1$s = $1.%1$s', TG_ARGV[0]);
        FOREACH _pk IN ARRAY TG_ARGV[1:] LOOP
            _where := _where || format(' AND _rev_table.%1$s = $1.%1$s', _pk);
        END LOOP;
    END IF;
    EXECUTE format('SELECT coalesce(max(rev_number), 0) FROM %s _rev_table WHERE %s', _rev_table, _where)
        INTO _rev_number
        USING _record;
    EXECUTE format('INSERT INTO %s VALUES ($1, $2, $3, $4.*)', _rev_table)
        USING _rev_number + 1, _op, now(), _record;
    RETURN _record;
END;
$$ LANGUAGE plpgsql SECURITY DEFINER;
select * from plpgsql_check_function('app_private.insert_revision', 'page');


drop trigger if exists _100_timestamps on public.page;
create trigger _100_timestamps
  before insert or update on page
  for each row
  execute procedure app_private.tg__timestamps();

drop trigger if exists _100_page_revision on public.page;
create trigger _100_page_revision
  after insert or update or delete on page
  for each row
  execute procedure app_private.insert_revision();

--! split: 30-authentication.sql
drop function if exists login(login varchar, passwd varchar) cascade;
create or replace function public.login(login varchar, passwd varchar, OUT couple pary, OUT sess session, out usr users) as $$
declare
  v_salt varchar;
begin
  if login like '%@%' then
    select users.* into usr from users where u_email = login limit 1;
  else
    select users.* into usr from users where u_login = login limit 1;
  end if;

  if usr is null then
    raise exception 'ACCOUNT_NOT_FOUND' using errcode = '28000';
  end if;

  select encode(digest('######TK.-.OLYMP######', 'md5'), 'hex') into v_salt;
  if usr.u_pass != encode(digest(v_salt || passwd || v_salt, 'sha1'), 'hex') then
    raise exception 'INVALID_PASSWORD' using errcode = '28P01';
  end if;

  if usr.u_ban then
    raise exception 'ACCOUNT_DISABLED' using errcode = '42501';
  end if;
  if not usr.u_confirmed then
    raise exception 'ACCOUNT_NOT_CONFIRMED' using errcode = '42501';
  end if;

  insert into session
    (ss_id, ss_user, ss_lifetime)
    values (gen_random_uuid(), usr.u_id, 86400)
    returning * into sess;

  select * from pary where p_archiv=false and (p_id_partner=usr.u_id or p_id_partnerka=usr.u_id) into couple;
end;
$$ language plpgsql strict volatile security definer;
select * from plpgsql_check_function('public.login');
grant execute on function login to anonymous;

create or replace function public.logout() returns void as $$
begin
  delete from session where ss_id=current_session_id();
end;
$$ language plpgsql strict volatile security definer;
select * from plpgsql_check_function('public.logout');
grant execute on function logout to anonymous;

create or replace function get_current_user() returns users as $$
  SELECT * FROM users WHERE u_id = nullif(current_setting('jwt.claims.user_id', true), '')::integer;
$$ language sql stable security definer;
grant execute on function get_current_user to anonymous;

create or replace function get_current_couple() returns pary as $$
  SELECT * FROM pary WHERE p_id in (select * from current_couple_ids()) limit 1;
$$ language sql stable security definer;
grant execute on function get_current_couple to anonymous;


create or replace function app_private.tg_users__notify_admin() returns trigger as $$
begin
  perform graphile_worker.add_job('notify_admin_registration', json_build_object('id', NEW.u_id));
  return NEW;
end;
$$ language plpgsql volatile security definer;
select * from plpgsql_check_function('app_private.tg_users__notify_admin', 'users');

drop trigger if exists _500_notify_admin ON users;
create trigger _500_notify_admin
  after insert on public.users
  for each row
  execute procedure app_private.tg_users__notify_admin();

create or replace function app_private.tg_users__encrypt_password() returns trigger as $$
declare
  v_salt varchar;
begin
  if length(NEW.u_pass) <> 40 then
      select encode(digest('######TK.-.OLYMP######', 'md5'), 'hex') into v_salt;
      NEW.u_pass := encode(digest(v_salt || NEW.u_pass || v_salt, 'sha1'), 'hex');
  end if;
  return NEW;
end;
$$ language plpgsql volatile;
select * from plpgsql_check_function('app_private.tg_users__encrypt_password', 'users');

drop trigger if exists _200_encrypt_password ON users;
create trigger _200_encrypt_password
  before insert or update on public.users
  for each row
  execute procedure app_private.tg_users__encrypt_password();

drop function if exists reset_password(login varchar, email varchar) cascade;
create or replace function public.reset_password(login varchar, email varchar) returns void as $$
declare
  v_salt varchar;
  usr users;
begin
  select * into usr from users where u_login=login and u_email=email;
  if usr is null then
    raise exception 'ACCOUNT_NOT_FOUND' using errcode = '28000';
  end if;
  perform graphile_worker.add_job('forgotten_password_generate', json_build_object('id', usr.u_id));
end;
$$ language plpgsql strict volatile security definer;
select * from plpgsql_check_function('public.reset_password');
grant execute on function public.reset_password to anonymous;


drop function if exists public.confirm_user(id bigint) cascade;
create or replace function public.confirm_user(id bigint, grp bigint, cohort bigint) returns void as $$
declare
  usr users;
begin
  select * into usr from users where u_id=id;
  if usr is null then
    raise exception 'ACCOUNT_NOT_FOUND' using errcode = '28000';
  end if;
  update users set u_confirmed=true, u_group=grp, u_skupina=cohort, u_system=false where u_id=id;
  perform graphile_worker.add_job('notify_confirmed_user', json_build_object('id', id));
end;
$$ language plpgsql strict volatile security definer;
select * from plpgsql_check_function('public.confirm_user');
grant execute on function public.confirm_user to administrator;

--! split: 60-gallery.sql
create or replace function public.title_videos() returns setof video as $$
  select * from video where v_id in (
    select pa_value::bigint from parameters where pa_name in (
      'title_video1', 'title_video2', 'title_video3', 'title_video4'
    )
  );
$$ language sql stable;

--! split: 70-members.sql
drop view if exists public.members cascade;
create or replace view public.members as
  WITH current_payments as (
    SELECT * from platby_item
    INNER JOIN platby_category ON pi_id_category=pc_id
    INNER JOIN platby_category_group ON pcg_id_category=pc_id
    INNER JOIN platby_group ON pg_id=pcg_id_group
    WHERE pg_type='1'
    AND CURRENT_DATE >= pc_valid_from
    AND CURRENT_DATE <= pc_valid_to
  )
  SELECT users.*, skupiny.*, (SELECT EXISTS (SELECT pi_id FROM current_payments WHERE pi_id_user=u_id)) as payment_valid
    FROM users INNER JOIN skupiny ON s_id=u_skupina
  WHERE u_confirmed='1'
    AND u_system='0'
    AND u_ban='0'
  ORDER BY s_name, u_email;

GRANT ALL ON public.members TO member;

create or replace function public.trainers() returns setof users as $$
  SELECT users.*
    FROM users INNER JOIN permissions on u_group=pe_id
  WHERE u_confirmed='1'
    AND u_system='0'
    AND u_ban='0'
    AND CASE
      WHEN (select pe_rozpis > 8 from current_permissions()) THEN pe_rozpis >= 8
      ELSE u_id = current_user_id()
    END
  ORDER BY u_prijmeni, u_jmeno;
$$ language sql stable;
GRANT EXECUTE ON function public.trainers TO member;

create or replace function my_lessons(start_date date, end_date date) returns setof rozpis_item as $$
  select rozpis_item.*
  from public.rozpis_item
  inner join public.rozpis on (rozpis.r_id = rozpis_item.ri_id_rodic)
  left join public.pary on (rozpis_item.ri_partner = pary.p_id)
  where (
        rozpis.r_trener = current_user_id()
     or pary.p_id_partner = current_user_id()
     or pary.p_id_partnerka = current_user_id()
  ) and rozpis.r_visible = true and r_datum >= start_date and r_datum <= end_date
  order by rozpis.r_datum, rozpis_item.ri_od
$$ language sql stable;
grant execute on function public.my_lessons TO member;

create or replace function public.schedules_for_range(start_date date, end_date date) returns setof rozpis as $$
  select * from rozpis
  where r_visible=true
  and r_datum >= start_date and r_datum <= end_date
  order by r_datum asc;
$$ language sql stable;
grant execute on function public.schedules_for_range TO member;

create or replace function public.reservations_for_range(start_date date, end_date date) returns setof nabidka as $$
  select * from nabidka
  where n_visible=true
  and n_od <= start_date and n_do >= end_date
  order by n_od asc;
$$ language sql stable;
grant execute on function public.reservations_for_range TO member;

create or replace function public.active_couples() returns setof pary as $$
  select p.*
  from pary as p
      left join users as m on p.p_id_partner=m.u_id
      left join users as f on p.p_id_partnerka=f.u_id
  where p.p_archiv = false
      and p.p_id_partner is not null and p.p_id_partner <> 0
      and p.p_id_partnerka is not null and p.p_id_partnerka <> 0
      and m.u_id is not null and f.u_id is not null
  order by m.u_prijmeni asc
$$ language sql stable;
grant execute on function public.active_couples TO member;


create or replace function public.create_couple(
  man bigint, woman bigint
) returns setof pary as $$
declare
  couple_man pary;
  couple_woman pary;
begin
  select * into couple_man from pary
  where p_archiv=false and p_id_partner=man;

  select * into couple_woman from pary
  where p_archiv=false and (p_id_partnerka=woman or (p_id_partnerka is null and p_id_partner=woman));

  if couple_man.p_id_partnerka = woman then
     return next couple_man;
  end if;

  if couple_man.p_id_partnerka is not null and couple_man.p_id_partnerka<>0 then
    insert into pary (p_id_partner, p_id_partnerka) VALUES (couple_man.p_id_partnerka, 0);
  end if;
  update pary set p_archiv=true where p_id = couple_man.p_id;

  if couple_woman.p_id_partnerka is not null and couple_woman.p_id_partnerka<>0 then
    insert into pary (p_id_partner, p_id_partnerka) VALUES (couple_woman.p_id_partner, 0);
  end if;
  update pary set p_archiv=true where p_id = couple_woman.p_id;

  return query insert into pary (p_id_partner, p_id_partnerka) VALUES (man, woman) returning *;
end;
$$ language plpgsql strict volatile security definer;
select * from plpgsql_check_function('public.create_couple');
grant execute on function public.create_couple to administrator;

create or replace function public.fix_unpaired_couples() returns setof pary as $$
  insert into pary (p_id_partner, p_id_partnerka)
  select u_id, 0 from users
  where u_id not in (
    select u_id from users
    left join pary on p_id_partnerka=u_id or p_id_partner=u_id
    where p_archiv=false
  ) returning *;
$$ language sql strict volatile security definer;
grant execute on function public.fix_unpaired_couples to administrator;


create or replace function public.book_lesson(lesson_id bigint) returns setof rozpis_item as $$
declare
  schedule rozpis;
  lesson rozpis_item;
  couple_id bigint;
begin
  select * into lesson from rozpis_item where ri_id=lesson_id;
  select * into schedule from rozpis where r_id=lesson.ri_id_rodic;
  select * into couple_id from current_couple_ids() limit 1;

  if schedule is null or lesson is null then
    raise exception 'ITEM_NOT_FOUND' using errcode = '28000';
  end if;

  if schedule.r_lock or lesson.ri_lock then
    raise exception 'ITEM_LOCKED' using errcode = '42501';
  end if;

  return query update rozpis_item set ri_partner = couple_id where ri_id = lesson_id
    returning *;
end;
$$ language plpgsql strict volatile security definer;
select * from plpgsql_check_function('public.book_lesson');
grant execute on function public.book_lesson to member;

create or replace function public.cancel_lesson(lesson_id bigint) returns setof rozpis_item as $$
declare
  schedule rozpis;
  lesson rozpis_item;
begin
  select * into lesson from rozpis_item where ri_id=lesson_id;
  select * into schedule from rozpis where r_id=lesson.ri_id_rodic;

  if schedule is null or lesson is null then
    raise exception 'ITEM_NOT_FOUND' using errcode = '28000';
  end if;

  if schedule.r_lock or lesson.ri_lock then
    raise exception 'ITEM_LOCKED' using errcode = '42501';
  end if;

  return query update rozpis_item set ri_partner = null where ri_id = lesson_id
    returning *;
end;
$$ language plpgsql strict volatile security definer;
select * from plpgsql_check_function('public.cancel_lesson');
grant execute on function public.cancel_lesson to member;

--! split: 80-crm.sql
do $$ begin
  if not exists (select 1 from information_schema.tables
    where table_schema = 'app_private' and table_name = 'crm_prospect'
  ) then

    create type app_private.crm_cohort as enum (
      'dancer',
      'hobbyist',
      'showdance',
      'free-lesson',
      'contact-me-later'
    );

    create type public.prospect_data as (
      name text,
      surname text,
      email text,
      phone text,
      yearofbirth text
    );

    create table app_private.crm_prospect (
      id serial primary key,
      cohort app_private.crm_cohort,
      data prospect_data,
      created_at timestamptz not null default now(),
      updated_at timestamptz not null default now()
    );
    create trigger _100_timestamps before insert or update on app_private.crm_prospect
      for each row execute procedure app_private.tg__timestamps();
    comment on table app_private.crm_prospect is E'@omit create';

    create table app_private.crm_activity (
      id serial primary key,
      prospect integer references app_private.crm_prospect (id),
      origin text not null,
      note text null,
      created_at timestamptz not null default now(),
      updated_at timestamptz not null default now()
    );
    create trigger _100_timestamps before insert or update on app_private.crm_activity
      for each row execute procedure app_private.tg__timestamps();
    comment on table app_private.crm_activity is E'@omit delete';
  end if;
end $$;

create or replace function public.prospect_form_dancer(
  cohort app_private.crm_cohort, prospect_data prospect_data, origin text, note text
) returns void as $$
declare
  prospect app_private.crm_prospect;
begin
  select * from app_private.crm_prospect where (data).email=prospect_data.email or (data).phone=prospect_data.phone into prospect;
  if prospect is null then
    insert into app_private.crm_prospect (cohort, data) values (cohort, prospect_data) returning * into prospect;
  end if;

  insert into app_private.crm_activity (prospect, origin, note) values (prospect.id, origin, note);
end;
$$ language plpgsql strict volatile security definer;
select * from plpgsql_check_function('public.prospect_form_dancer');

drop function if exists active_prospects();
create or replace function active_prospects() returns table (
  id bigint, data prospect_data, cohort app_private.crm_cohort, updated_at timestamptz
) AS $$
  SELECT crm_prospect.id, crm_prospect.data, crm_prospect.cohort, crm_prospect.updated_at
  FROM app_private.crm_prospect
  ORDER BY crm_prospect.updated_at DESC
$$ language sql stable security definer;

GRANT ALL ON FUNCTION public.active_prospects TO administrator;
