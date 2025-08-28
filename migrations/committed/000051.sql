--! Previous: sha1:99cdcacecaed0ba0827a87ec2b8732a0109fd38b
--! Hash: sha1:3c44c5ff02b87ddd5e4330bdd010aa675c8290b8

--! split: 1-current.sql
drop table if exists room_attachment;
drop table if exists room;
drop table if exists location_attachment;
drop table if exists location;
drop table if exists tenant_attachment;
drop type if exists tenant_attachment_type;
drop table if exists app_private.skupiny;
drop table if exists app_private.pary_navrh;
drop table if exists app_private.crm_activity;
drop table if exists app_private.crm_prospect;
drop type if exists app_private.crm_prospect;
drop type if exists app_private.crm_cohort;
