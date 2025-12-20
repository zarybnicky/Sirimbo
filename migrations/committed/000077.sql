--! Previous: sha1:d2aad9aaa111c8ed6088b873b5149575c9e828dd
--! Hash: sha1:c334ad738be675ccb5a665326dee02ca41d3a138

--! split: 1-current.sql
drop type if exists public.prospect_data cascade;

drop function if exists post_without_cache;
drop function if exists fetch_with_cache(input_url text, headers http_header[]);

drop table if exists public.response_cache;

drop table if exists public.pghero_query_stats;
drop table if exists public.pghero_space_stats;

alter table if exists public.platby_category_group set schema app_private;
alter table if exists public.platby_group_skupina set schema app_private;

grant usage on schema federated to anonymous;
GRANT SELECT ON ALL TABLES IN SCHEMA federated TO anonymous;
ALTER DEFAULT PRIVILEGES IN SCHEMA federated GRANT SELECT ON TABLES TO anonymous;

drop function if exists csts_athlete(text);
drop function if exists csts_athlete(int);
create or replace function csts_athlete(idt int) returns text as $$
  select canonical_name
  from federated.person
  join federated.athlete on person.id = athlete.person_id
  join federated.federation_athlete on athlete.id = federation_athlete.athlete_id
  where federation = 'csts' and external_id = idt::text;
$$ language sql stable;

create or replace function wdsf_athlete(min int) returns text as $$
  select canonical_name
  from federated.person
  join federated.athlete on person.id = athlete.person_id
  join federated.federation_athlete on athlete.id = federation_athlete.athlete_id
  where federation = 'wdsf' and external_id = min::text;
$$ language sql stable;

grant all on function csts_athlete to anonymous;
grant all on function wdsf_athlete to anonymous;

drop function if exists person_csts_progress;
create or replace function person_csts_progress(in_person public.person) returns table (
  competitor_name text,
  category federated.category,
  points numeric(10, 3),
  finals integer
) as $$
  select
    competitor.name as competitor_name,
    row(category.*) as category,
    ccp.points,
    ccp.domestic_finale + ccp.foreign_finale as finals
  from federated.federation_athlete fa
  join federated.athlete on athlete.id = fa.athlete_id
  join federated.competitor_component cp on cp.athlete_id = athlete.id
  join federated.competitor on competitor.id = cp.competitor_id
  join federated.competitor_category_progress ccp on competitor.id = ccp.competitor_id and fa.federation = ccp.federation
  join federated.category on ccp.category_id = category.id
  where fa.federation = 'csts' and fa.external_id = in_person.csts_id;
$$ language sql stable;

comment on function person_csts_progress is '@simpleCollections only';
grant all on function person_csts_progress to anonymous;
