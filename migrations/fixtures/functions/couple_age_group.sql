create or replace function couple_age_group(c couple) returns text
language sql stable as $$
  select r.age_group_code
  from person m, person w, federated.couple_age_rule r
  where m.id = c.man_id and w.id = c.woman_id
    and m.birth_date is not null and w.birth_date is not null
    and r.division = 'General'
    and r.age_group_code <> 'Under_21'
    and r.older_age @> (extract(year from current_date)::int - extract(year from least(m.birth_date, w.birth_date))::int)
    and r.younger_age @> (extract(year from current_date)::int - extract(year from greatest(m.birth_date, w.birth_date))::int)
  order by lower(r.older_age) desc nulls last, upper(r.older_age)
  limit 1;
$$;

comment on function couple_age_group is '@behavior -queryField';
grant execute on function couple_age_group to anonymous;
