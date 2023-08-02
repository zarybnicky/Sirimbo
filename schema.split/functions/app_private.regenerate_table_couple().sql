CREATE FUNCTION app_private.regenerate_table_couple() RETURNS void
    LANGUAGE plpgsql STRICT SECURITY DEFINER
    AS $$
declare
  row pary;
  man person;
  woman person;
begin
  delete from couple;
  for row in select * from pary LOOP
    if row.p_id_partnerka is null then continue; end if;
    select * into man from person where legacy_user_id = row.p_id_partner;
    select * into woman from person where legacy_user_id = row.p_id_partnerka;
    insert into couple (legacy_pary_id, man_id, woman_id, since, until, active)
    values (row.p_id, man.id, woman.id, row.p_timestamp_add, row.p_timestamp_archive, not row.p_archiv);
  end loop;
end;
$$;



