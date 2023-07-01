CREATE FUNCTION public.active_couples() RETURNS SETOF public.pary
    LANGUAGE sql STABLE
    AS $$
  select p.*
  from pary as p
      left join users as m on p.p_id_partner=m.u_id
      left join users as f on p.p_id_partnerka=f.u_id
  where p.p_archiv = false
      and p.p_id_partner is not null and p.p_id_partner <> 0
      and p.p_id_partnerka is not null and p.p_id_partnerka <> 0
      and m.u_id is not null and f.u_id is not null
  order by m.u_prijmeni asc
$$;

GRANT ALL ON FUNCTION public.active_couples() TO member;


