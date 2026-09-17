create or replace function orphaned_documents() returns setof document
  language sql stable
as $$
  select * from document
  where num_nonnulls(event_instance_id, event_series_id, cohort_id) = 0
  order by updated_at desc
$$;

comment on function orphaned_documents() is '@simpleCollections only';

grant execute on function orphaned_documents() to anonymous;
