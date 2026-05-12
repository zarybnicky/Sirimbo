CREATE FUNCTION public.person_csts_progress(in_person public.person) RETURNS TABLE(competitor_name text, category federated.category, points numeric, finals integer)
    LANGUAGE sql STABLE
    AS $$
select
  competitor.name as competitor_name,
  row(category.*) as category,
  ccp.points,
  ccp.domestic_finals + ccp.foreign_finals as finals
from federated.person p
       join federated.competitor_component cp on cp.person_id = p.id
       join federated.competitor on competitor.id = cp.competitor_id
       join federated.competitor_category_progress ccp on competitor.id = ccp.competitor_id
       join federated.category on ccp.category_id = category.id
where p.federation = 'csts'
  and p.external_id = in_person.csts_id::bigint;
$$;

COMMENT ON FUNCTION public.person_csts_progress(in_person public.person) IS '@simpleCollections only';

GRANT ALL ON FUNCTION public.person_csts_progress(in_person public.person) TO anonymous;
