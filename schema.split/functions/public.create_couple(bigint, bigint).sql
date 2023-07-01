CREATE FUNCTION public.create_couple(man bigint, woman bigint) RETURNS SETOF public.pary
    LANGUAGE plpgsql STRICT SECURITY DEFINER
    AS $$
declare
  couple_man pary;
  couple_woman pary;
begin
  select * into couple_man from pary
  where p_archiv=false and p_id_partner=man;

  select * into couple_woman from pary
  where p_archiv=false and (p_id_partnerka=woman or (p_id_partnerka is null and p_id_partner=woman));

  if couple_man.p_id_partnerka = woman then
     return next couple_man;
  end if;

  if couple_man.p_id_partnerka is not null and couple_man.p_id_partnerka<>0 then
    insert into pary (p_id_partner, p_id_partnerka) VALUES (couple_man.p_id_partnerka, 0);
  end if;
  update pary set p_archiv=true where p_id = couple_man.p_id;

  if couple_woman.p_id_partnerka is not null and couple_woman.p_id_partnerka<>0 then
    insert into pary (p_id_partner, p_id_partnerka) VALUES (couple_woman.p_id_partner, 0);
  end if;
  update pary set p_archiv=true where p_id = couple_woman.p_id;

  return query insert into pary (p_id_partner, p_id_partnerka) VALUES (man, woman) returning *;
end;
$$;

GRANT ALL ON FUNCTION public.create_couple(man bigint, woman bigint) TO administrator;


