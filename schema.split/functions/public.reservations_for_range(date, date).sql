CREATE FUNCTION public.reservations_for_range(start_date date, end_date date) RETURNS SETOF public.nabidka
    LANGUAGE sql STABLE
    AS $$
  select * from nabidka
  where n_visible=true
  and n_do >= start_date and n_od <= end_date
  order by n_od asc;
$$;

GRANT ALL ON FUNCTION public.reservations_for_range(start_date date, end_date date) TO member;


