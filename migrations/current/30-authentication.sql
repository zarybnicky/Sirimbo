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
select plpgsql_check_function('public.login');
grant execute on function login to anonymous;

create or replace function public.logout() returns void as $$
begin
  delete from session where ss_id=current_session_id();
end;
$$ language plpgsql strict volatile security definer;
select plpgsql_check_function('public.logout');
grant execute on function logout to anonymous;

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

create or replace function current_session_id() returns text as $$
  select nullif(current_setting('jwt.claims.user_id', true), '')::integer;
$$ language sql stable;
grant execute on function current_session_id to anonymous;

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

create or replace function get_current_user() returns users as $$
  SELECT * FROM users WHERE u_id = nullif(current_setting('jwt.claims.user_id', true), '')::integer;
$$ language sql stable;
grant execute on function get_current_user to anonymous;


create or replace function app_private.tg_users__notify_admin() returns trigger as $$
begin
  perform graphile_worker.add_job('notify_admin_registration', json_build_object('id', NEW.u_id));
  return NEW;
end;
$$ language plpgsql volatile security definer;
select plpgsql_check_function('app_private.tg_users__notify_admin', 'users');

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
select plpgsql_check_function('app_private.tg_users__encrypt_password', 'users');

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
select plpgsql_check_function('public.reset_password');
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
select plpgsql_check_function('public.confirm_user');
grant execute on function public.confirm_user to administrator;
