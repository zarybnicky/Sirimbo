CREATE FUNCTION app_private.regenerate_table_person() RETURNS void
    LANGUAGE plpgsql STRICT SECURITY DEFINER
    AS $$
declare
  row users;
  address_id bigint;
  person_id bigint;
  tenant_id bigint = 1::bigint;
begin
  delete from person;
  FOR row IN SELECT * FROM users where not exists (select 1 from person where legacy_user_id=u_id) LOOP
    insert into person (legacy_user_id, first_name, last_name, gender, birth_date, tax_identification_number, nationality)
    values (
      row.u_id,
      row.u_jmeno,
      row.u_prijmeni,
      (case row.u_pohlavi when 'm' then 'man' else 'woman' end)::gender_type,
      row.u_narozeni,
      row.u_rodne_cislo,
      row.u_nationality
    ) returning id into person_id;

    insert into address (street, conscription_number, orientation_number, district, city, postal_code)
    values (row.u_street, row.u_conscription_number, row.u_orientation_number, row.u_district, row.u_city, row.u_postal_code)
    returning id into address_id;

    insert into person_address (person_id, address_id) values (person_id, address_id);
    insert into person_email (person_id, email) values (person_id, row.u_email);
    insert into person_phone (person_id, phone) values (person_id, row.u_telefon);
    insert into user_proxy (user_id, person_id) values (row.u_id, person_id);
    insert into tenant_membership (tenant_id, person_id, since, until, active)
    values (
      tenant_id,
      person_id,
      COALESCE(row.u_member_since, row.u_created_at),
      case row.u_ban when true then row.u_timestamp else null end,
      not row.u_ban
    );
    insert into cohort_membership (cohort_id, person_id, since, until, active)
    values (
      row.u_skupina,
      person_id,
      COALESCE(row.u_member_since, row.u_created_at),
      case row.u_ban when true then row.u_timestamp else null end,
      not row.u_ban
    );

    if exists (select * from rozpis where r_trener = row.u_id) then
      insert into tenant_trainer (tenant_id, person_id, since, until, active)
      values (tenant_id, person_id, COALESCE(row.u_member_since, row.u_created_at), NULL, not row.u_ban);
    end if;

    if row.u_group in (1, 9) then
      insert into tenant_administrator (tenant_id, person_id, since, until, active)
      values (tenant_id, person_id, COALESCE(row.u_member_since, row.u_created_at), NULL, not row.u_ban);
    end if;
  end loop;
end;
$$;



