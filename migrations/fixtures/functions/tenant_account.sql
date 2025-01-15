CREATE or replace FUNCTION tenant_account(c text, OUT acc account) RETURNS account LANGUAGE plpgsql AS $$
begin
  select * into acc from account
  where person_id is null and currency=c and tenant_id=current_tenant_id();

  if not found then
    insert into account (tenant_id, person_id, currency)
    values (current_tenant_id(), null, coalesce(c, 'CZK'))
    on conflict on constraint account_tenant_id_person_id_currency_idx do nothing
    returning * into acc;
  end if;
end;
$$ security definer;

select verify_function('tenant_account');
grant all on function tenant_account to anonymous;
