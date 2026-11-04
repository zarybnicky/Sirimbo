
drop function if exists public.person_is_admin(public.person);
drop function if exists public.person_is_trainer(public.person);
drop function if exists public.person_is_member(public.person);

drop function if exists public.person_cohort_ids(public.person);
drop function if exists public.payment_debtor_is_unpaid(public.payment_debtor);
drop function if exists public.account_balance(public.account);
drop function if exists public.tenant_membership_tenant_name(public.tenant_membership);
drop function if exists public.tenant_trainer_tenant_name(public.tenant_trainer);
drop function if exists public.tenant_administrator_tenant_name(public.tenant_administrator);
drop function if exists public.access_credential_is_allowed(public.access_credential);
