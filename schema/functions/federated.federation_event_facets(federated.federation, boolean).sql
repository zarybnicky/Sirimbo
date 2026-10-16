CREATE FUNCTION federated.federation_event_facets(federation federated.federation, include_cancelled boolean DEFAULT true) RETURNS federated.federation_event_facets
    LANGUAGE sql STABLE
    AS $$
  select row(array(
    select row(
      extract(year from e.start_date)::integer,
      count(*) filter (where include_cancelled is distinct from false or e.status <> 'cancelled')::integer,
      count(*) filter (
        where (include_cancelled is distinct from false or e.status <> 'cancelled')
          and e.venue_lat is not null and e.venue_lng is not null
      )::integer
    )::federated.federation_event_year
    from federated.event e
    where e.federation = federation.code
    group by extract(year from e.start_date)
    order by extract(year from e.start_date) desc
  ))::federated.federation_event_facets;
$$;

REVOKE ALL ON FUNCTION federated.federation_event_facets(federation federated.federation, include_cancelled boolean) FROM PUBLIC;
GRANT ALL ON FUNCTION federated.federation_event_facets(federation federated.federation, include_cancelled boolean) TO anonymous;
