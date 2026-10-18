create or replace function app_private.refresh_competitor_activity(competitor_ids text[])
returns void language plpgsql as $$
begin
  if coalesce(cardinality(competitor_ids), 0) = 0 then
    return;
  end if;

  perform c.id from federated.competitor c where c.id = any(competitor_ids) order by c.id for no key update;

  with activity as (
    select c.id,
      (select max(comp.start_date)
       from federated.competition_result r
       join federated.competition comp on comp.id = r.competition_id
       where r.competitor_id = c.id) as latest_result_at,
      (select max(comp.start_date)
       from federated.competition_entry e
       join federated.competition comp on comp.id = e.competition_id
       where e.competitor_id = c.id and not e.cancelled
         and not exists (
           select 1 from federated.competition_result r
           where (r.competition_id, r.competitor_id) = (e.competition_id, e.competitor_id)
         )) as latest_entry_at
    from federated.competitor c
    where c.id = any(competitor_ids)
  )
  update federated.competitor c
  set latest_result_at = a.latest_result_at, latest_entry_at = a.latest_entry_at
  from activity a
  where c.id = a.id
    and (c.latest_result_at, c.latest_entry_at) is distinct from
      (a.latest_result_at, a.latest_entry_at);
end;
$$;

create or replace function app_private.tg_competitor_activity__refresh()
returns trigger language plpgsql as $$
-- @plpgsql_check_options: oldtable = oldtable, newtable = newtable
declare
  competitor_ids text[];
begin
  if TG_OP = 'INSERT' then
    select array_agg(distinct competitor_id) into competitor_ids from newtable;
  elsif TG_OP = 'DELETE' then
    select array_agg(distinct competitor_id) into competitor_ids from oldtable;
  else
    select array_agg(competitor_id) into competitor_ids
    from (select competitor_id from oldtable union select competitor_id from newtable) affected;
  end if;
  perform app_private.refresh_competitor_activity(competitor_ids);
  return null;
end;
$$;

create or replace trigger _900_refresh_competitor_activity_insert
  after insert on federated.competition_result
  referencing new table as newtable
  for each statement execute function app_private.tg_competitor_activity__refresh();
create or replace trigger _900_refresh_competitor_activity_update
  after update on federated.competition_result
  referencing old table as oldtable new table as newtable
  for each statement execute function app_private.tg_competitor_activity__refresh();
create or replace trigger _900_refresh_competitor_activity_delete
  after delete on federated.competition_result
  referencing old table as oldtable
  for each statement execute function app_private.tg_competitor_activity__refresh();

create or replace trigger _900_refresh_competitor_activity_insert
  after insert on federated.competition_entry
  referencing new table as newtable
  for each statement execute function app_private.tg_competitor_activity__refresh();
create or replace trigger _900_refresh_competitor_activity_update
  after update on federated.competition_entry
  referencing old table as oldtable new table as newtable
  for each statement execute function app_private.tg_competitor_activity__refresh();
create or replace trigger _900_refresh_competitor_activity_delete
  after delete on federated.competition_entry
  referencing old table as oldtable
  for each statement execute function app_private.tg_competitor_activity__refresh();

create or replace function app_private.tg_competition_dates__refresh_competitor_activity()
returns trigger language plpgsql as $$
declare
  competitor_ids text[];
begin
  with changed as (
    select n.id from newtable n join oldtable o using (id)
    where n.start_date is distinct from o.start_date
  ), affected as (
    select r.competitor_id from federated.competition_result r join changed c on c.id = r.competition_id
    union
    select e.competitor_id from federated.competition_entry e join changed c on c.id = e.competition_id
  )
  select array_agg(competitor_id) into competitor_ids from affected;
  perform app_private.refresh_competitor_activity(competitor_ids);
  return null;
end;
$$;

create or replace trigger _900_refresh_competitor_activity_dates
  after update on federated.competition
  referencing old table as oldtable new table as newtable
  for each statement execute function app_private.tg_competition_dates__refresh_competitor_activity();
