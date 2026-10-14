create or replace function app_private.tg_resolve_age_group() returns trigger
language plpgsql as $$
begin
  new.age_group_code := federated.resolve_age_group(new.age_group);
  return new;
end;
$$;
comment on function app_private.tg_resolve_age_group() is '@behavior -*';
revoke all on function app_private.tg_resolve_age_group() from public;
