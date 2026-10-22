CREATE FUNCTION app_private.refresh_competitor_activity(competitor_ids text[]) RETURNS void
    LANGUAGE plpgsql
    AS $$
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
