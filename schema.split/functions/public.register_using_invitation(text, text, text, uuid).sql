CREATE FUNCTION public.register_using_invitation(email text, login text, passwd text, token uuid, OUT usr public.users, OUT sess public.session, OUT jwt public.jwt_token) RETURNS record
    LANGUAGE plpgsql STRICT SECURITY DEFINER
    AS $$
declare
  invitation person_invitation;
  v_salt text;
begin
  select * into invitation from person_invitation where access_token=token;

  if invitation is null then
    raise exception 'INVITATION_NOT_FOUND' using errcode = '28000';
  end if;
  if invitation.used_at is not null then
    raise exception 'INVITATION_ALREADY_USED' using errcode = '28P01';
  end if;

  select encode(digest('######TK.-.OLYMP######', 'md5'), 'hex') into v_salt;
  insert into users (u_login, u_email, u_pass) values (login, email, encode(digest(v_salt || passwd || v_salt, 'sha1'), 'hex')) returning * into usr;
  insert into user_proxy (user_id, person_id) values (usr.id, invitation.person_id);
  update person_invitation set used_at=now() where access_token=token;
  insert into session (ss_id, ss_user, ss_lifetime) values (gen_random_uuid(), usr.u_id, 86400)
  returning * into sess;
  jwt := app_private.create_jwt_token(usr);
end
$$;

GRANT ALL ON FUNCTION public.register_using_invitation(email text, login text, passwd text, token uuid, OUT usr public.users, OUT sess public.session, OUT jwt public.jwt_token) TO anonymous;


