CREATE FUNCTION public.tenant_account(c text, OUT acc public.account) RETURNS public.account
    LANGUAGE sql SECURITY DEFINER
    AS $$
  with inserted as (
    insert into account (tenant_id, person_id, currency)
    values (current_tenant_id(), null, coalesce(c, 'CZK'))
    on conflict (tenant_id, person_id, currency) do nothing
    returning *
  )
  select * from inserted union select * from account where person_id is null and currency=c;
$$;

GRANT ALL ON FUNCTION public.tenant_account(c text, OUT acc public.account) TO anonymous;


