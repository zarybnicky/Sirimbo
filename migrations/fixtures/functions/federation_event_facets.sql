drop function if exists federated.federation_event_facets;
drop type if exists federated.federation_event_facets;
drop type if exists federated.federation_event_year;

create type federated.federation_event_year as (
  year integer,
  event_count integer,
  mapped_event_count integer
);
create type federated.federation_event_facets as (
  years federated.federation_event_year[]
);

comment on column federated.federation_event_year.year is '@notNull';
comment on column federated.federation_event_year.event_count is '@notNull';
comment on column federated.federation_event_year.mapped_event_count is '@notNull';
comment on column federated.federation_event_facets.years is '@notNull';

create function federated.federation_event_facets(federation federated.federation)
returns federated.federation_event_facets
language sql stable as $$
  select row(array(
    select row(
      extract(year from e.start_date)::integer,
      count(*)::integer,
      count(*) filter (where e.venue_lat is not null and e.venue_lng is not null)::integer
    )::federated.federation_event_year
    from federated.event e
    where e.federation = federation.code
    group by extract(year from e.start_date)
    order by extract(year from e.start_date) desc
  ))::federated.federation_event_facets;
$$;

revoke all on function federated.federation_event_facets from public;
grant execute on function federated.federation_event_facets to anonymous;
