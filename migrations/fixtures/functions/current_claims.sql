create or replace function public.current_claims() returns jsonb
    language sql stable security definer
as $$
  select to_jsonb(app_private.create_jwt_token(users)) - 'exp'
  from users
  where id = nullif(current_setting('jwt.claims.user_id', true), '')::bigint;
$$;

grant all on function public.current_claims() to anonymous;
