CREATE FUNCTION public.person_account(p_id bigint, c text, OUT acc public.account) RETURNS public.account
    LANGUAGE sql SECURITY DEFINER
    AS $$
  with inserted as (
    insert into account (tenant_id, person_id, currency)
    values (current_tenant_id(), p_id, coalesce(c, 'CZK'))
    on conflict (tenant_id, person_id, currency) do nothing
    returning *
  )
  select * from inserted union select * from account where person_id=p_id and currency=c and tenant_id=current_tenant_id();
$$;

GRANT ALL ON FUNCTION public.person_account(p_id bigint, c text, OUT acc public.account) TO anonymous;


