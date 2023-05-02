--! Previous: sha1:b53ddebac5716a7f96fa1559c07ab9af217d890d
--! Hash: sha1:11ee8ffb56ed076b16d1d8a2064758fedaa9cf3c

select app_private.drop_policies('public.event');
alter table event enable row level security;
create policy manage_all on event to administrator using (true) with check (true);
create policy select_member on event for select to member using (true);
create policy select_public on event for select to anonymous using (is_public = true);
grant all on event to anonymous;

comment on view public.akce_item is E'@foreignKey (ai_id_rodic) references akce (a_id)';

grant all on function get_current_tenant to anonymous;

alter table aktuality alter column at_kat set default '1';
alter table event alter column files_legacy set default '';

alter table tenant alter column member_info type text using member_info::text;
alter table skupiny alter column internal_info type text using internal_info::text;
alter table cohort_group alter column description type text using description::text;

DROP VIEW IF EXISTS akce CASCADE;
alter table event alter column summary type text using summary::text;
CREATE VIEW akce AS
  SELECT
    id            as a_id,
    name          as a_jmeno,
    location_text as a_kde,
    description   as a_info,
    since         as a_od,
    until         as a_do,
    capacity      as a_kapacita,
    files_legacy  as a_dokumenty,
    updated_at    as a_timestamp,
    is_locked     as a_lock,
    is_visible    as a_visible,

    summary,
    is_public,
    enable_notes
  FROM event;
