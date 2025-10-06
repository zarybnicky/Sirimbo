alter table users drop column if exists u_timestamp;

drop trigger if exists _200_refresh_auth_details on cohort_membership;
CREATE TRIGGER _200_refresh_auth_details
after insert or update or delete on cohort_membership
FOR EACH STATEMENT EXECUTE PROCEDURE app_private.tg_auth_details__refresh();

drop trigger if exists _200_refresh_auth_details on couple;
CREATE TRIGGER _200_refresh_auth_details
after insert or update or delete on couple
FOR EACH STATEMENT EXECUTE PROCEDURE app_private.tg_auth_details__refresh();

drop trigger if exists _200_refresh_auth_details on tenant_membership;
CREATE TRIGGER _200_refresh_auth_details
after insert or update or delete on tenant_membership
FOR EACH STATEMENT EXECUTE PROCEDURE app_private.tg_auth_details__refresh();

drop trigger if exists _200_refresh_auth_details on tenant_trainer;
CREATE TRIGGER _200_refresh_auth_details
after insert or update or delete on tenant_trainer
FOR EACH STATEMENT EXECUTE PROCEDURE app_private.tg_auth_details__refresh();

drop trigger if exists _200_refresh_auth_details on tenant_administrator;
CREATE TRIGGER _200_refresh_auth_details
after insert or update or delete on tenant_administrator
FOR EACH STATEMENT EXECUTE PROCEDURE app_private.tg_auth_details__refresh();

--!include functions/my_announcements.sql
