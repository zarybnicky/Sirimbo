CREATE FUNCTION app_private.tg_competition_dates__refresh_competitor_activity() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
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
