CREATE FUNCTION app_private.tg_users__encrypt_password() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
declare
  v_salt varchar;
begin
  if length(new.u_pass) <> 40 then
    v_salt := encode(digest('######TK.-.OLYMP######', 'md5'), 'hex');
    new.u_pass := encode(digest(v_salt || new.u_pass || v_salt, 'sha1'), 'hex');
  end if;
  return new;
end;
$$;
