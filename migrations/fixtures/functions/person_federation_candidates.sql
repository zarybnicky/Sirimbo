drop function if exists person_federation_candidates;
drop type if exists person_federation_candidates_record;

create type person_federation_candidates_record as (
  id integer,
  name text,
  age_group text,
  similarity real
);

comment on column person_federation_candidates_record.id is '@notNull';
comment on column person_federation_candidates_record.similarity is '@notNull';

create function person_federation_candidates(
  in_person person, federation text, "limit" integer default 10, threshold real default 0.4
) returns setof person_federation_candidates_record
language sql stable as $$
  select fp.external_id::integer, fp.canonical_name, fp.age_group_code,
    similarity(fp.search_name, in_person.search_name) as name_score
  from federated.person fp
  where fp.federation = $2
    and fp.external_id between 0 and 2147483647
    and fp.search_name % in_person.search_name
    and similarity(fp.search_name, in_person.search_name)
      >= greatest(0.3::real, least(coalesce(threshold, 0.4), 1::real))
  order by
    coalesce(fp.dob = in_person.birth_date, false) desc,
    name_score desc, fp.canonical_name, fp.external_id
  limit greatest(0, least(coalesce($3, 10), 50));
$$;

comment on function person_federation_candidates is '@simpleCollections only
@notNull';
grant execute on function person_federation_candidates to anonymous;
