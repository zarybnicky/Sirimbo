-- The current session's claims as plain data, for clients that authenticate via
-- the httpOnly cookie and can't read the JWT. Same shape as the token payload by
-- construction (minus exp); note to_jsonb renders bigint[] as JSON numbers.
create or replace function public.current_claims() returns jsonb
    language sql stable security definer
as $$
  select to_jsonb(app_private.create_jwt_token(users)) - 'exp'
  from users
  where id = nullif(current_setting('jwt.claims.user_id', true), '')::integer;
$$;

grant all on function public.current_claims() to anonymous;
