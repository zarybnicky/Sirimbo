CREATE FUNCTION public.update_tenant_settings_key(path text[], new_value jsonb) RETURNS public.tenant_settings
    LANGUAGE sql
    AS $$
  update tenant_settings
  set settings = jsonb_set(settings, path, new_value, true)
  where tenant_id=current_tenant_id()
  returning *;
$$;

GRANT ALL ON FUNCTION public.update_tenant_settings_key(path text[], new_value jsonb) TO administrator;
