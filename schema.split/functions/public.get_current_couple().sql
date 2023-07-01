CREATE FUNCTION public.get_current_couple() RETURNS public.pary
    LANGUAGE sql STABLE SECURITY DEFINER
    AS $$
  SELECT * FROM pary WHERE p_id in (select * from current_couple_ids()) limit 1;
$$;

GRANT ALL ON FUNCTION public.get_current_couple() TO anonymous;


