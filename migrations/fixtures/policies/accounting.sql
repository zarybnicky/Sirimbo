select app_private.drop_policies('public.account');
create policy current_tenant on account as restrictive
  using (tenant_id = (select current_tenant_id()));
create policy admin_manage on account to administrator using (true);
create policy member_view on account for select to member
  using (person_id = any ((select current_person_ids())::bigint[]));

select app_private.drop_policies('public.payment');
create policy current_tenant on payment as restrictive
  using (tenant_id = (select current_tenant_id()));
create policy admin_manage on payment to administrator using (true);
create policy member_view on payment for select to member
  using (id = any (array(select app_private.visible_payment_ids())));

select app_private.drop_policies('public.payment_debtor');
create policy current_tenant on payment_debtor as restrictive
  using (tenant_id = (select current_tenant_id()));
create policy admin_manage on payment_debtor to administrator using (true);
create policy member_view on payment_debtor for select to member
  using (person_id = any ((select public.current_person_ids())::bigint[]));

select app_private.drop_policies('payment_recipient');
create policy current_tenant on payment_recipient as restrictive
  using (tenant_id = (select current_tenant_id()));
create policy admin_manage on payment_recipient to administrator using (true);
create policy member_view on payment_recipient for select to member
  using (account_id = any (array(select public.current_account_ids())));

select app_private.drop_policies('posting');
create policy current_tenant on posting as restrictive
  using (tenant_id = (select current_tenant_id()));
create policy admin_manage on posting to administrator using (true);
create policy member_view on posting for select to member
  using (account_id = any (array(select current_account_ids())));

select app_private.drop_policies('public.transaction');
create policy current_tenant on transaction as restrictive
  using (tenant_id = (select current_tenant_id()));
create policy admin_manage on transaction to administrator using (true);
create policy member_view on transaction for select to member
  using (id = any (array(select transaction_id from posting))
    or payment_id = any (array(select app_private.visible_payment_ids())));

grant all on account, payment, payment_debtor, payment_recipient, posting, transaction to anonymous;
