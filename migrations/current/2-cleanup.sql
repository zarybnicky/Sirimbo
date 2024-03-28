
drop function if exists app_private.tg__person_email_primary;
drop function if exists app_private.tg__person_address_primary;
drop function if exists app_private.tg__person_phone_primary;
drop function if exists public.users_in_public_cohort;
drop function if exists users_date_of_oldest_payment;
drop function if exists users_date_of_newest_payment;

comment on function skupiny_in_current_tenant is E'@filterable
@deprecated';

-- rename from up_timestamp, up_timestamp_add
-- drop old triggers
-- drop old trigger functions
-- rename to created_at, updated_ad
-- apply new trigger
-- add generated columns


-- on_update_event_timestamp
-- updated_at

-- on_update_current_timestamp_users
-- u_timestamp

-- on_update_current_timestamp_upozorneni
-- up_timestamp

-- on_update_current_timestamp_rozpis
-- r_timestamp

-- on_update_current_timestamp_nabidka
-- n_timestamp

-- on_update_current_timestamp_galerie_foto
-- gf_timestamp

-- on_update_current_timestamp_dokumenty
-- d_timestamp

-- on_update_current_timestamp_aktuality
-- at_timestamp

-- skupiny_in_current_tenant
-- -> function tenant_cohorts(tenant)
