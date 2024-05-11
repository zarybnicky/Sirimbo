do $$
begin
  if exists (select 1 from pg_tables where schemaname = 'public' and tablename='skupiny') then
    alter table skupiny set schema app_private;
  end if;
end
$$;

GRANT ALL ON TABLE public.cohort TO anonymous;
ALTER TABLE public.cohort ENABLE ROW LEVEL SECURITY;
drop index if exists event_lesson_demand_registration_id_idx;
drop index if exists event_trainer_event_id_idx;

comment on column aktuality.at_kat is '@deprecated';
comment on column account.name is '@deprecated';
comment on column upozorneni.up_barvy is '@deprecated';
comment on column person.middle_name is '@deprecated';

create or replace function event_trainer_name(t event_trainer) returns text stable language sql
begin atomic
  select person_name(person.*) from person where t.person_id=person.id;
end;
grant all on function event_trainer_name to anonymous;

create or replace function event_instance_trainer_name(t event_instance_trainer) returns text stable language sql
begin atomic
  select person_name(person.*) from person where t.person_id=person.id;
end;
grant all on function event_instance_trainer_name to anonymous;

drop index if exists event_attendance_tenant_id_idx;
drop index if exists event_registration_tenant_id_idx;
drop index if exists event_instance_until_idx;
drop index if exists event_trainer_tenant_id_idx;
drop index if exists idx_pr_tenant;
drop index if exists idx_gf_tenant;
drop index if exists idx_pi_tenant;
drop index if exists event_lesson_demand_tenant_id_idx;
drop index if exists couple_man_id_idx;
drop index if exists couple_status_idx;
drop index if exists couple_woman_id_idx;
drop index if exists d_kategorie;
drop index if exists d_timestamp;
drop index if exists event_target_cohort_tenant_id_idx;
drop index if exists idx_24575_aktuality_at_foto_main_fkey;
drop index if exists idx_24593_dokumenty_d_kdo_fkey;
drop index if exists idx_24602_gd_id_rodic;
drop index if exists idx_24662_pary_navrh_pn_navrhl_fkey;
drop index if exists idx_24662_pary_navrh_pn_partner_fkey;
drop index if exists idx_24662_pary_navrh_pn_partnerka_fkey;
drop index if exists idx_24690_platby_category_group_pcg_id_category_fkey;
drop index if exists idx_24707_platby_group_skupina_pgs_id_group_fkey;
drop index if exists idx_24777_upozorneni_skupiny_ups_id_skupina_fkey;
drop index if exists is_public;
drop index if exists ordering;
drop index if exists idx_24613_gf_id_rodic;
drop index if exists idx_24713_platby_item_pi_id_category_fkey;
DROP INDEX if exists uploaded_by; -- 16 kB, Never Used Indexes, table attachment
DROP INDEX if exists idx_cg_tenant; -- 16 kB, Never Used Indexes, table cohort_group
DROP INDEX if exists idx_d_tenant; -- 16 kB, Never Used Indexes, table dokumenty
DROP INDEX if exists idx_fr_tenant; -- 16 kB, Never Used Indexes, table form_responses
DROP INDEX if exists type; -- 16 kB, Never Used Indexes, table form_responses
DROP INDEX if exists updated_at; -- 16 kB, Never Used Indexes, table form_responses
DROP INDEX if exists idx_gd_tenant; -- 16 kB, Never Used Indexes, table galerie_dir
DROP INDEX if exists idx_ot_tenant; -- 16 kB, Never Used Indexes, table otp_token
DROP INDEX if exists otp_token_user_id_idx; -- 16 kB, Never Used Indexes, table otp_token
DROP INDEX if exists app_private.idx_24662_pary_navrh_pn_navrhl_fkey; -- 16 kB, Never Used Indexes, table app_private.pary_navrh
DROP INDEX if exists app_private.idx_24662_pary_navrh_pn_partner_fkey; -- 16 kB, Never Used Indexes, table app_private.pary_navrh
DROP INDEX if exists app_private.idx_24662_pary_navrh_pn_partnerka_fkey; -- 16 kB, Never Used Indexes, table app_private.pary_navrh
DROP INDEX if exists idx_pei_tenant; -- 16 kB, Never Used Indexes, table person_invitation
DROP INDEX if exists person_invitation_person_id_idx; -- 16 kB, Never Used Indexes, table person_invitation
DROP INDEX if exists idx_pc_tenant; -- 16 kB, Never Used Indexes, table platby_category
DROP INDEX if exists idx_pcg_tenant; -- 16 kB, Never Used Indexes, table platby_category_group
DROP INDEX if exists idx_pg_tenant; -- 16 kB, Never Used Indexes, table platby_group
DROP INDEX if exists idx_pgs_tenant; -- 16 kB, Never Used Indexes, table platby_group_skupina
DROP INDEX if exists room_location_idx; -- 16 kB, Never Used Indexes, table room
DROP INDEX if exists app_private.idx_sk_tenant; -- 16 kB, Never Used Indexes, table app_private.skupiny
DROP INDEX if exists app_private.s_visible; -- 16 kB, Never Used Indexes, table app_private.skupiny
DROP INDEX if exists app_private.skupiny_cohort_group_idx; -- 16 kB, Never Used Indexes, table app_private.skupiny
DROP INDEX if exists app_private.skupiny_ordering_idx; -- 16 kB, Never Used Indexes, table app_private.skupiny
DROP INDEX if exists tenant_administrator_active_idx; -- 16 kB, Never Used Indexes, table tenant_administrator
DROP INDEX if exists tenant_administrator_status_idx; -- 16 kB, Never Used Indexes, table tenant_administrator
DROP INDEX if exists tenant_administrator_tenant_id_idx; -- 16 kB, Never Used Indexes, table tenant_administrator
DROP INDEX if exists tenant_location_tenant_id_idx; -- 16 kB, Never Used Indexes, table tenant_location
DROP INDEX if exists tenant_trainer_active_idx; -- 16 kB, Never Used Indexes, table tenant_trainer
DROP INDEX if exists tenant_trainer_status_idx; -- 16 kB, Never Used Indexes, table tenant_trainer
DROP INDEX if exists tenant_trainer_tenant_id_idx; -- 16 kB, Never Used Indexes, table tenant_trainer
DROP INDEX if exists user_proxy_active_idx; -- 16 kB, Never Used Indexes, table user_proxy
DROP INDEX if exists user_proxy_status_idx; -- 16 kB, Never Used Indexes, table user_proxy
DROP INDEX if exists idx_us_tenant; -- 16 kB, Never Used Indexes, table users
