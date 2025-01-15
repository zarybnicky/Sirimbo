CREATE or replace FUNCTION person_account(p_id bigint, c text, OUT acc account) RETURNS account LANGUAGE plpgsql AS $$
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
$$ security definer;

select verify_function('person_account');
grant all on function person_account to anonymous;
