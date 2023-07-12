drop function if exists title_videos();
drop function if exists app_private.tg__tenant();
drop table if exists app_private.video_source;
drop table if exists app_private.video_list;
drop table if exists app_private.video;
drop table if exists public.page_revision;
drop table if exists public.page;
drop table if exists app_private.page_revision;
drop table if exists app_private.page;

UPDATE pary SET p_id_partnerka=NULL where p_id_partnerka=0;
DELETE FROM pary WHERE p_id_partnerka = 29;
DELETE FROM pary WHERE p_id_partnerka = 33;
DELETE FROM pary WHERE p_id_partnerka = 83;
DELETE FROM pary WHERE p_id_partnerka = 104;
DELETE FROM pary WHERE p_id_partnerka = 132;
DELETE FROM pary WHERE p_id_partnerka = 147;
ALTER TABLE pary DROP CONSTRAINT IF EXISTS pary_p_id_partnerka_fkey;
ALTER TABLE pary ADD CONSTRAINT pary_p_id_partnerka_fkey FOREIGN KEY (p_id_partnerka) REFERENCES users (u_id);

comment on table pary is E'@omit create, update, delete';

drop function if exists delete_couple(id bigint);
CREATE or replace FUNCTION public.delete_couple(couple_id bigint) RETURNS SETOF public.pary LANGUAGE plpgsql STRICT SECURITY DEFINER AS $$
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
select verify_function('delete_couple');
GRANT ALL ON FUNCTION public.delete_couple(bigint) TO administrator;


create or replace function legacy_duplicate_nabidka(nabidka_id bigint) RETURNS public.nabidka LANGUAGE plpgsql STRICT as $$
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
select verify_function('legacy_duplicate_nabidka');
GRANT ALL ON FUNCTION public.legacy_duplicate_nabidka(bigint) TO anonymous;

create or replace function legacy_duplicate_rozpis(rozpis_id bigint) RETURNS public.rozpis LANGUAGE plpgsql STRICT as $$
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
select verify_function('legacy_duplicate_rozpis');
GRANT ALL ON FUNCTION public.legacy_duplicate_rozpis(bigint) TO anonymous;
