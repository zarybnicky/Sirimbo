-- Write your migration here

drop function if exists delete_couple;
drop function if exists get_current_couple;

COMMENT ON TABLE public.couple IS '@omit delete';

select app_private.drop_policies('public.address');
alter table person_address drop column if exists address_id;

drop function if exists person_primary_address;
CREATE or replace FUNCTION public.person_primary_address(p public.person) RETURNS public.person_address
    LANGUAGE sql STABLE
    AS $$
  select person_address.* from person_address where person_id = p.id and is_primary = true;
$$;


drop table if exists nabidka_item;
drop table if exists nabidka;
drop table if exists rozpis_item;
drop table if exists rozpis;
drop table if exists address;
drop table if exists attendee_user;
drop table if exists pary;
drop table if exists permissions;

drop function if exists confirm_user(bigint, bigint, bigint);

drop type if exists pary_p_lat_trida;
drop type if exists pary_p_stt_trida;

alter table users drop column if exists u_group;
alter table users drop column if exists u_skupina;
alter table users drop column if exists u_narozeni;
alter table users drop column if exists u_rodne_cislo;
alter table users drop column if exists u_street;
alter table users drop column if exists u_conscription_number;
alter table users drop column if exists u_orientation_number;
alter table users drop column if exists u_district;
alter table users drop column if exists u_city;
alter table users drop column if exists u_postal_code;
alter table users drop column if exists u_lock;
alter table users drop column if exists u_level;
alter table users drop column if exists u_dancer;
alter table users drop column if exists u_teacher;
