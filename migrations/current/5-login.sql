
drop function if exists public.login;
CREATE FUNCTION public.login(login character varying, passwd character varying, OUT sess public.session, OUT usr public.users) RETURNS record LANGUAGE plpgsql STRICT SECURITY DEFINER AS $$
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

  insert into session (ss_id, ss_user, ss_lifetime) values (gen_random_uuid(), usr.u_id, 86400)
  returning * into sess;
end;
$$;
GRANT ALL ON FUNCTION public.login(character varying, character varying, OUT usr public.users) TO anonymous;
