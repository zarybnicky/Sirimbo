CREATE FUNCTION public.tenant_account(c text, OUT acc public.account) RETURNS public.account
    LANGUAGE sql SECURITY DEFINER
    AS $$
  with inserted as (
    insert into account (tenant_id, person_id, currency)
    values (current_tenant_id(), null, coalesce(c, 'CZK'))
    on conflict on constraint account_tenant_id_person_id_currency_idx do nothing
    returning *
  )
  select * from inserted union select * from account where person_id is null and currency=c and tenant_id=current_tenant_id();
$$;

GRANT ALL ON FUNCTION public.tenant_account(c text, OUT acc public.account) TO anonymous;


