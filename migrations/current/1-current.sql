
grant all on function immutable_concat_ws to anonymous;
grant all on function http_header to anonymous;
grant all on function http to trainer;
grant all on function fetch_with_cache to trainer;

--!include functions/tg_cohort_membership__on_status.sql


create or replace function app_private.merge_couples(one bigint, two bigint) returns void language plpgsql as $$
declare
  registrations_before bigint;
  registrations_after bigint;
begin
  -- duplicate detection: select array_agg(couple.id), man_id, woman_id, array_agg(array[since, until] order by since) from couple group by man_id, woman_id having count(*) > 1;

  select count(*) into registrations_before from event_registration;

  assert (select min(man_id) is not distinct from max(man_id) from couple where id in (one, two));
  assert (select min(woman_id) is not distinct from max(woman_id) from couple where id in (one, two));
  assert (select extract(epoch from (select since from couple where id = two) - (select until from couple where id = one)) < 3600);

  update event_registration set couple_id = one where couple_id = two;
  update couple set until = (select until from couple where id = two) where id = one;
  delete from couple where id = two;

  select count(*) into registrations_after from event_registration;
  assert registrations_before = registrations_after;
end
$$;

do $$ begin
  if not exists (select 1 from pg_type where typname = 'address_type' and typcategory = 'C') then
    create type address_type as (
      street text,
      conscription_number text,
      orientation_number text,
      district text,
      city text,
      region text,
      postal_code text
    );
    create domain address_domain as address_type check (
      value is null or (
        (value).street is not null and
        (value).conscription_number is not null and
        (value).orientation_number is not null and
        (value).city is not null and
        (value).region is not null and
        (value).postal_code is not null
      )
    );
  end if;
end $$;

alter table person add column if not exists address address_domain;

do $$ begin
  if exists (select 1 from pg_tables where tablename = 'legacy_users' and true) then
    update person set address = (u_street, u_conscription_number, u_orientation_number, u_district, u_city, '', u_postal_code)
      from legacy_users where legacy_user_id = u_id and address is null;
    drop table legacy_users;
  end if;
end $$;
