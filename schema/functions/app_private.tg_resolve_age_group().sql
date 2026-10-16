CREATE FUNCTION app_private.tg_resolve_age_group() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
begin
  new.age_group_code := federated.resolve_age_group(new.age_group);
  return new;
end;
$$;

COMMENT ON FUNCTION app_private.tg_resolve_age_group() IS '@behavior -*';

REVOKE ALL ON FUNCTION app_private.tg_resolve_age_group() FROM PUBLIC;
