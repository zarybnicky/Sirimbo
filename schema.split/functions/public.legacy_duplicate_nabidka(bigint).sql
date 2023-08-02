CREATE FUNCTION public.legacy_duplicate_nabidka(nabidka_id bigint) RETURNS public.nabidka
    LANGUAGE plpgsql STRICT
    AS $$
declare
  new nabidka;
begin
  INSERT INTO nabidka (n_trener, n_pocet_hod, n_max_pocet_hod, n_od, n_do, n_visible, n_lock)
  SELECT n_trener, n_pocet_hod, n_max_pocet_hod, n_od, n_do, n_visible, n_lock FROM nabidka WHERE n_id=nabidka_id
  RETURNING * into new;

  INSERT INTO nabidka_item (ni_partner, ni_id_rodic, ni_pocet_hod)
  SELECT ni_partner, new.id, ni_pocet_hod FROM nabidka_item WHERE ni_id_rodic = nabidka_id;

  RETURN new;
end;
$$;

GRANT ALL ON FUNCTION public.legacy_duplicate_nabidka(nabidka_id bigint) TO anonymous;


