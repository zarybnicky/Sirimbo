CREATE FUNCTION public.access_credential_last_used(c public.access_credential) RETURNS timestamp with time zone
    LANGUAGE sql STABLE
    AS $$
  select max(e.occurred_at)
  from access_event e
  where e.tenant_id = c.tenant_id
    and e.kind = c.kind
    and e.code = c.code
    and e.allowed
    and c.valid_range @> e.occurred_at;
$$;

GRANT ALL ON FUNCTION public.access_credential_last_used(c public.access_credential) TO anonymous;
