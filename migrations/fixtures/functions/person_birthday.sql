create or replace function person_birthday(p person, since date default current_date)
returns date language sql stable strict as $$
  -- Adding a year interval clamps February 29 to February 28 when needed.
  select case
    when (p.birth_date + make_interval(years => extract(year from since)::int - extract(year from p.birth_date)::int))::date >= since
    then (p.birth_date + make_interval(years => extract(year from since)::int - extract(year from p.birth_date)::int))::date
    else (p.birth_date + make_interval(years => extract(year from since)::int - extract(year from p.birth_date)::int + 1))::date
  end;
$$;

comment on function person_birthday is '@behavior -queryField';
grant execute on function person_birthday to anonymous;
