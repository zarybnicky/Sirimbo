CREATE FUNCTION public.current_couple_ids() RETURNS SETOF bigint
    LANGUAGE sql STABLE
    AS $$
  select distinct p_id
  from public.pary
  where p_id_partner = current_user_id() and p_archiv = false
  UNION
  select distinct p_id
  from public.pary
  where p_id_partnerka = current_user_id() and p_archiv = false;
$$;

GRANT ALL ON FUNCTION public.current_couple_ids() TO anonymous;


