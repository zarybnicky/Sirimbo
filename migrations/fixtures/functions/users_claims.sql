create or replace function users_claims(u users) returns jsonb
  language sql stable strict
  security definer
  set search_path = pg_catalog, public, pg_temp
as $$
  select to_jsonb(app_private.create_jwt_token(actual_user)) - 'exp' - 'tenant_id'
  from users actual_user
  where actual_user.id = u.id
    and (u.id = current_user_id()
      or pg_has_role(current_setting('role'), 'administrator', 'member'));
$$;

comment on function users_claims is '@behavior +typeField -queryField';
grant execute on function users_claims to anonymous;
