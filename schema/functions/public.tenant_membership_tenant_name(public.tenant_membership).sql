CREATE FUNCTION public.tenant_membership_tenant_name(public.tenant_membership) RETURNS text
    LANGUAGE sql STABLE
    AS $_$
  select name from tenant where id = $1.tenant_id;
$_$;
