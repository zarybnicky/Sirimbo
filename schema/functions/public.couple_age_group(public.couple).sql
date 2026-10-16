CREATE FUNCTION public.couple_age_group(c public.couple) RETURNS text
    LANGUAGE sql STABLE
    AS $$
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

COMMENT ON FUNCTION public.couple_age_group(c public.couple) IS '@behavior -queryField';

GRANT ALL ON FUNCTION public.couple_age_group(c public.couple) TO anonymous;
