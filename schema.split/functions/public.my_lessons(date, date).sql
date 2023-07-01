CREATE FUNCTION public.my_lessons(start_date date, end_date date) RETURNS SETOF public.rozpis_item
    LANGUAGE sql STABLE
    AS $$
  select rozpis_item.*
  from public.rozpis_item
  inner join public.rozpis on (rozpis.r_id = rozpis_item.ri_id_rodic)
  left join public.pary on (rozpis_item.ri_partner = pary.p_id)
  where (
        rozpis.r_trener = current_user_id()
     or pary.p_id_partner = current_user_id()
     or pary.p_id_partnerka = current_user_id()
  ) and rozpis.r_visible = true and r_datum >= start_date and r_datum <= end_date
  order by rozpis.r_datum, rozpis_item.ri_od
$$;

GRANT ALL ON FUNCTION public.my_lessons(start_date date, end_date date) TO member;


