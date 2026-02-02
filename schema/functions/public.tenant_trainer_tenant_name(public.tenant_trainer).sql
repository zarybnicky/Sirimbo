CREATE FUNCTION public.tenant_trainer_tenant_name(public.tenant_trainer) RETURNS text
    LANGUAGE sql STABLE
    AS $_$
  select name from tenant where id = $1.tenant_id;
$_$;
