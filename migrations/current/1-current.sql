alter type jwt_token
  alter attribute my_person_ids type bigint[],
  alter attribute my_tenant_ids type bigint[],
  alter attribute my_cohort_ids type bigint[],
  alter attribute my_couple_ids type bigint[];

drop function if exists public.my_tenant_ids;

alter table event
  drop column if exists guest_price,
  drop column if exists member_price,
  drop column if exists title_image_legacy,
  drop column if exists description_member;

alter table event_instance
  drop column if exists location_id;

alter table payment
  drop column if exists tags;

--!include functions/otp_login.sql
--!include functions/register_to_event_many.sql
--!include functions/register_using_invitation.sql
--!include functions/register_without_invitation.sql
--!include functions/reset_password.sql
--!include functions/tenant_account.sql
--!include functions/create_jwt_token.sql

--!include federated-full-rebuild.sql
