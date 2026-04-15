CREATE FUNCTION federated.replace_competitor_category_progress(in_federation text, in_competitor_id bigint, in_entries federated.competitor_category_progress_input[] DEFAULT '{}'::federated.competitor_category_progress_input[]) RETURNS void
    LANGUAGE plpgsql
    SET search_path TO 'federated', 'pg_temp'
    AS $$
begin
  delete from federated.competitor_category_progress
  where federation = in_federation
    and competitor_id = in_competitor_id;

  insert into federated.competitor_category_progress (
    federation,
    competitor_id,
    category_id,
    points,
    domestic_finale,
    foreign_finale
  )
  select
    in_federation,
    in_competitor_id,
    (entry).category_id,
    coalesce((entry).points, 0),
    coalesce((entry).domestic_finale, 0),
    coalesce((entry).foreign_finale, 0)
  from unnest(coalesce(in_entries, '{}'::federated.competitor_category_progress_input[])) as entry
  where (entry).category_id is not null;
end;
$$;
