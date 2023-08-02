CREATE FUNCTION public.legacy_duplicate_rozpis(rozpis_id bigint) RETURNS public.rozpis
    LANGUAGE plpgsql STRICT
    AS $$
declare
  new rozpis;
begin
  INSERT INTO rozpis (r_trener, r_datum, r_kde, r_visible, r_lock)
  SELECT r_trener, r_datum, r_kde, r_visible, r_lock FROM rozpis WHERE r_id=rozpis_id
  RETURNING * into new;

  INSERT INTO rozpis_item (ri_id_rodic, ri_partner, ri_od, ri_do, ri_lock)
  SELECT new.id, ri_partner, ri_od, ri_do, ri_lock FROM rozpis_item WHERE ri_id_rodic = rozpis_id;

  RETURN new;
end;
$$;

GRANT ALL ON FUNCTION public.legacy_duplicate_rozpis(rozpis_id bigint) TO anonymous;


