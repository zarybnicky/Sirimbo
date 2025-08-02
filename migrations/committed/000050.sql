--! Previous: sha1:2195f336fd0e46f2177fbd8ee0ac7fa222e01ef2
--! Hash: sha1:99cdcacecaed0ba0827a87ec2b8732a0109fd38b

--! split: 1-current.sql
drop materialized view if exists allowed_tenants;
drop view if exists allowed_tenants_view;

CREATE or replace FUNCTION app_private.tg_auth_details__refresh() RETURNS TRIGGER security definer AS $$
BEGIN
  refresh materialized view concurrently auth_details;
  return null;
END
$$ LANGUAGE plpgsql;

drop trigger if exists _200_refresh_auth_details on cohort_membership;
CREATE TRIGGER _200_refresh_auth_details
after insert or update or delete on cohort_membership
FOR EACH ROW EXECUTE PROCEDURE app_private.tg_auth_details__refresh();

drop trigger if exists _200_refresh_auth_details on couple;
CREATE TRIGGER _200_refresh_auth_details
after insert or update or delete on couple
FOR EACH ROW EXECUTE PROCEDURE app_private.tg_auth_details__refresh();

drop trigger if exists _200_refresh_auth_details on tenant_membership;
CREATE TRIGGER _200_refresh_auth_details
after insert or update or delete on tenant_membership
FOR EACH ROW EXECUTE PROCEDURE app_private.tg_auth_details__refresh();

drop trigger if exists _200_refresh_auth_details on tenant_trainer;
CREATE TRIGGER _200_refresh_auth_details
after insert or update or delete on tenant_trainer
FOR EACH ROW EXECUTE PROCEDURE app_private.tg_auth_details__refresh();

drop trigger if exists _200_refresh_auth_details on tenant_administrator;
CREATE TRIGGER _200_refresh_auth_details
after insert or update or delete on tenant_administrator
FOR EACH ROW EXECUTE PROCEDURE app_private.tg_auth_details__refresh();

alter table users alter column u_login drop not null;
