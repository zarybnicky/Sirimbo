drop function if exists person_federation_candidates;
drop function if exists federation_person_candidates;
drop type if exists person_federation_candidates_record;

create type person_federation_candidates_record as (
  id integer,
  name text,
  age_group text,
  similarity real,
  birth_year integer,
  csts_id integer,
  wdsf_id integer,
  is_strong_match boolean
);

comment on column person_federation_candidates_record.id is '@notNull';
comment on column person_federation_candidates_record.similarity is '@notNull';
comment on column person_federation_candidates_record.is_strong_match is '@notNull';

create function federation_person_candidates(
  first_name text, last_name text, federation text,
  gender gender_type default null, birth_date date default null,
  "limit" integer default 10, threshold real default 0.4
) returns setof person_federation_candidates_record
language sql stable as $$
  with input as materialized (
    select app_private.normalize_name(concat_ws(' ', trim($1), trim($2))) as name,
      app_private.normalize_name(trim($1)) as first_name,
      app_private.normalize_name(trim($2)) as last_name,
      extract(year from $5)::integer as birth_year,
      extract(year from current_date)::integer - extract(year from $5)::integer as age,
      case $4 when 'man' then 'male'::federated.gender
        when 'woman' then 'female'::federated.gender end as gender
  ), matches as (
    select fp.*, similarity(fp.search_name, i.name) as name_score,
      nullif(fp.gender, 'unknown') = i.gender as gender_match,
      coalesce(fp.birth_year, extract(year from fp.dob)::integer) = i.birth_year
        or (fp.birth_year is null and fp.dob is null
          and coalesce(g.age, g.person_age) @> i.age) as age_match
    from federated.person fp
    cross join input i
    left join federated.age_group g on g.code = fp.age_group_code
    where fp.federation = $3
      and nullif(i.first_name, '') is not null and nullif(i.last_name, '') is not null
      and fp.external_id between 1 and 2147483647
      and fp.search_name % i.name
      and similarity(fp.search_name, i.name)
        >= greatest(0.3::real, least(coalesce(threshold, 0.4), 1::real))
      -- Indexed word matching uses pg_trgm's default 0.6 threshold for each name part.
      and fp.search_name %> i.first_name
      and fp.search_name %> i.last_name
      and (i.gender is null or fp.gender is null or fp.gender = 'unknown' or fp.gender = i.gender)
      and (
        i.birth_year is null
        or coalesce(fp.birth_year, extract(year from fp.dob)::integer) = i.birth_year
        or (fp.birth_year is null and fp.dob is null and (
          coalesce(g.age, g.person_age) is null
          or coalesce(g.age, g.person_age) @> i.age
          -- Adult is also a broad label for people eligible for senior categories.
          or (g.code = 'Adult' and i.age >= lower(g.age))
        ))
      )
  ), candidates as (
    select * from matches
    order by gender_match desc nulls last, age_match desc nulls last,
      name_score desc, canonical_name, external_id
    limit greatest(0, least(coalesce($6, 10), 50))
  )
  select fp.external_id::integer, fp.canonical_name, fp.age_group_code, fp.name_score,
    coalesce(fp.birth_year, extract(year from fp.dob)::integer),
    case fp.federation when 'csts' then fp.external_id::integer else linked.external_id end,
    case fp.federation when 'wdsf' then fp.external_id::integer else linked.external_id end,
    fp.name_score = 1 and fp.gender_match is true and fp.age_match is true
      and not exists (
        select 1 from matches other
        where other.external_id <> fp.external_id and other.name_score = 1
      )
  from candidates fp
  left join lateral (
    select min(other.external_id)::integer as external_id
    from federated.person other
    where (
      (fp.federation = 'wdsf' and other.federation = 'csts'
        and other.external_id::text = fp.national_reference)
      or (fp.federation = 'csts' and other.federation = 'wdsf'
        and other.national_reference = fp.external_id::text)
    )
      and other.external_id between 1 and 2147483647
      and similarity(other.search_name, fp.search_name) >= 0.8
      and (other.gender is null or other.gender = 'unknown'
        or fp.gender is null or fp.gender = 'unknown' or other.gender = fp.gender)
      and (coalesce(other.birth_year, extract(year from other.dob)::integer) is null
        or coalesce(fp.birth_year, extract(year from fp.dob)::integer) is null
        or coalesce(other.birth_year, extract(year from other.dob)::integer)
          = coalesce(fp.birth_year, extract(year from fp.dob)::integer))
    having count(*) = 1
  ) linked on true
  order by fp.gender_match desc nulls last, fp.age_match desc nulls last,
    fp.name_score desc, fp.canonical_name, fp.external_id;
$$;

comment on function federation_person_candidates is '@behavior +list -connection
@notNull';
grant execute on function federation_person_candidates to anonymous;

create function person_federation_candidates(
  in_person person, federation text, "limit" integer default 10, threshold real default 0.4
) returns setof person_federation_candidates_record
language sql stable as $$
  select * from federation_person_candidates(
    in_person.first_name, in_person.last_name, $2, in_person.gender, in_person.birth_date, $3, $4
  );
$$;

comment on function person_federation_candidates is '@behavior +list -connection
@notNull';
grant execute on function person_federation_candidates to anonymous;
