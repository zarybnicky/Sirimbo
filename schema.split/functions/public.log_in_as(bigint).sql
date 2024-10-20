CREATE FUNCTION public.log_in_as(id bigint, OUT usr public.users, OUT jwt public.jwt_token) RETURNS record
    LANGUAGE plpgsql STRICT SECURITY DEFINER
    AS $_$
begin
  select users.* into usr from users where users.id=$1;
  jwt := app_private.create_jwt_token(usr);
end
$_$;

GRANT ALL ON FUNCTION public.log_in_as(id bigint, OUT usr public.users, OUT jwt public.jwt_token) TO administrator;
