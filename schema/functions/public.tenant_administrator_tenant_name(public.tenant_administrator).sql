CREATE FUNCTION public.tenant_administrator_tenant_name(public.tenant_administrator) RETURNS text
    LANGUAGE sql STABLE
    AS $_$
  select name from tenant where id = $1.tenant_id;
$_$;
