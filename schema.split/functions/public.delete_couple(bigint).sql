CREATE FUNCTION public.delete_couple(couple_id bigint) RETURNS SETOF public.pary
    LANGUAGE plpgsql STRICT SECURITY DEFINER
    AS $$
declare
  couple pary;
begin
  update pary set p_archiv = true where p_id = couple_id returning * into couple;

  insert into pary (p_id_partner, p_id_partnerka) VALUES (couple.p_id_partner, null);
  if couple.p_id_partnerka is not null and couple.p_id_partnerka <> 0 then
    insert into pary (p_id_partner, p_id_partnerka) VALUES (couple.p_id_partner, null);
  end if;
  return next couple;
end;
$$;

GRANT ALL ON FUNCTION public.delete_couple(couple_id bigint) TO administrator;


