CREATE FUNCTION public.person_csts_progress(in_person public.person) RETURNS TABLE(competitor_name text, category federated.category, points numeric, finals integer)
    LANGUAGE sql STABLE
    AS $$
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
$$;

COMMENT ON FUNCTION public.person_csts_progress(in_person public.person) IS '@simpleCollections only';

GRANT ALL ON FUNCTION public.person_csts_progress(in_person public.person) TO anonymous;
