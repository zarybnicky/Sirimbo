create or replace function person_age_group(p person) returns text
language sql stable as $$
  select code from federated.age_group
  where person_age @> (extract(year from current_date)::int - extract(year from p.birth_date)::int);
$$;

comment on function person_age_group is '@behavior -queryField';
grant execute on function person_age_group to anonymous;
