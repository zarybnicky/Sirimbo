CREATE FUNCTION public.fix_unpaired_couples() RETURNS SETOF public.pary
    LANGUAGE sql STRICT SECURITY DEFINER
    AS $$
  insert into pary (p_id_partner, p_id_partnerka)
  select u_id, 0 from users
  where u_id not in (
    select u_id from users
    left join pary on p_id_partnerka=u_id or p_id_partner=u_id
    where p_archiv=false
  ) returning *;
$$;

GRANT ALL ON FUNCTION public.fix_unpaired_couples() TO administrator;


