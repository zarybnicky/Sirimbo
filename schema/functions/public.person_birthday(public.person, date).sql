CREATE FUNCTION public.person_birthday(p public.person, since date DEFAULT CURRENT_DATE) RETURNS date
    LANGUAGE sql STABLE STRICT
    AS $$
  -- Adding a year interval clamps February 29 to February 28 when needed.
  select case
    when (p.birth_date + make_interval(years => extract(year from since)::int - extract(year from p.birth_date)::int))::date >= since
    then (p.birth_date + make_interval(years => extract(year from since)::int - extract(year from p.birth_date)::int))::date
    else (p.birth_date + make_interval(years => extract(year from since)::int - extract(year from p.birth_date)::int + 1))::date
  end;
$$;

COMMENT ON FUNCTION public.person_birthday(p public.person, since date) IS '@behavior -queryField';

GRANT ALL ON FUNCTION public.person_birthday(p public.person, since date) TO anonymous;
