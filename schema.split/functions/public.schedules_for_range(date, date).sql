CREATE FUNCTION public.schedules_for_range(start_date date, end_date date) RETURNS SETOF public.rozpis
    LANGUAGE sql STABLE
    AS $$
  select * from rozpis
  where r_visible=true
  and r_datum >= start_date and r_datum <= end_date
  order by r_datum asc;
$$;

GRANT ALL ON FUNCTION public.schedules_for_range(start_date date, end_date date) TO member;


