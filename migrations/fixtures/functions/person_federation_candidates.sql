create or replace function public.person_federation_candidates(
  in_person public.person, federation text, "limit" integer default 10, threshold real default 0.4
) returns table (id integer, name text, age_group text, similarity real)
language sql stable as $$
  select fp.external_id::integer, fp.canonical_name, fp.age_group,
    public.similarity(fp.search_name, in_person.search_name) as name_score
  from federated.person fp
  where fp.federation = $2
    and fp.external_id between 0 and 2147483647
    and fp.search_name % in_person.search_name
    and public.similarity(fp.search_name, in_person.search_name)
      >= greatest(0.3::real, least(coalesce(threshold, 0.4), 1::real))
  order by
    coalesce(fp.dob = in_person.birth_date, false) desc,
    name_score desc, fp.canonical_name, fp.external_id
  limit greatest(0, least(coalesce($3, 10), 50));
$$;

comment on function public.person_federation_candidates is '@simpleCollections only';
grant execute on function public.person_federation_candidates to anonymous;
