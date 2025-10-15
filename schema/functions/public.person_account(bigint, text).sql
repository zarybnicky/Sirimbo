CREATE FUNCTION public.person_account(p_id bigint, c text, OUT acc public.account) RETURNS public.account
    LANGUAGE plpgsql SECURITY DEFINER
    SET search_path TO 'pg_catalog', 'public', 'pg_temp'
    AS $$
begin
  select * into acc from account
  where person_id=p_id and currency=c and tenant_id=current_tenant_id();

  if not found then
    insert into account (tenant_id, person_id, currency)
    values (current_tenant_id(), p_id, coalesce(c, 'CZK'))
    on conflict on constraint account_tenant_id_person_id_currency_idx do nothing
    returning * into acc;
  end if;
end;
$$;

GRANT ALL ON FUNCTION public.person_account(p_id bigint, c text, OUT acc public.account) TO anonymous;
