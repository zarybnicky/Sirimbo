
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
