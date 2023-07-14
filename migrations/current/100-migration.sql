
do $$
declare
  row users;
  gender gender_type;
  address_id bigint;
  person_id bigint;
  tenant_id bigint = 1;
begin
  delete from person;
  FOR row IN SELECT * FROM users LOOP
    select case row.u_pohlavi when 'm' then 'man' else 'woman' end into gender;
    insert into person (legacy_user_id, first_name, last_name, gender, birth_date, tax_identification_number, nationality)
    values (row.u_id, row.u_jmeno, row.u_prijmeni, gender, row.u_narozeni, row.u_rodne_cislo, row.u_nationality)
    returning id into person_id;

    insert into address (street, conscription_number, orientation_number, district, city, postal_code)
    values (row.u_street, row.u_conscription_number, row.u_orientation_number, row.u_district, row.u_city, row.u_postal_code)
    returning id into address_id;

    insert into person_address (person_id, address_id) values (person_id, address_id);
    insert into person_email (person_id, email) values (person_id, row.u_email);
    insert into person_phone (person_id, phone) values (person_id, row.u_telefon);
    insert into user_proxy (user_id, person_id) values (row.u_id, person_id);
    insert into tenant_membership (tenant_id, person_id, since, until, active)
    values (tenant_id, person_id, COALESCE(row.u_member_since, row.u_created_at), case row.u_ban when true then row.u_timestamp else null end, not row.u_ban);
    insert into cohort_membership (cohort_id, person_id, since, until, active)
    values (row.u_skupina, person_id, COALESCE(row.u_member_since, row.u_created_at), case row.u_ban when true then row.u_timestamp else null end, not row.u_ban);

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

do $$
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

delete from event where "type" != 'camp';

do $$
declare
  schedule nabidka;
  lesson nabidka_item;
  item event;
  par pary;
  trainer event_trainer;
  reg event_registration;
begin
  for schedule in select * from nabidka loop
    insert into event (name, description, location_text, type, since, until, capacity, is_locked, is_visible)
    values ('', '', '', 'reservation', schedule.n_od + time '00:00', schedule.n_do + time '23:59:59', schedule.n_pocet_hod, schedule.n_lock, schedule.n_visible)
    returning * into item;

    insert into event_trainer (event_id, person_id)
    select item.id, person.id from person where legacy_user_id = schedule.n_trener
    returning * into trainer;

    for lesson in select * from nabidka_item where ni_id_rodic = schedule.n_id loop
      select * into par from pary where p_id = lesson.ni_partner;
      if par.p_id_partnerka is null then
        insert into event_registration (event_id, person_id, is_confirmed)
        select item.id, person.id, true from person where legacy_user_id = par.p_id_partner
        returning * into reg;
        insert into event_lesson_demand (registration_id, trainer_id, lesson_count)
        values (reg.id, trainer.id, lesson.ni_pocet_hod);
      else
        insert into event_registration (event_id, couple_id, is_confirmed)
        select item.id, couple.id, true from couple where legacy_pary_id = lesson.ni_partner
        returning * into reg;
        insert into event_lesson_demand (registration_id, trainer_id, lesson_count)
        values (reg.id, trainer.id, lesson.ni_pocet_hod);
      end if;
    end loop;
  end loop;
end;
$$;

do $$
declare
  schedule rozpis;
  lesson rozpis_item;
  item event;
  par pary;
begin
  for lesson in select * from rozpis_item loop
    select * into schedule from rozpis where r_id = lesson.ri_id_rodic;

    insert into event (name, description, type, since, until, location_text, is_locked, is_visible)
    values ('', '', 'lesson', schedule.r_datum + lesson.ri_od, schedule.r_datum + lesson.ri_do, schedule.r_kde, schedule.r_lock, schedule.r_visible)
    returning * into item;

    insert into event_trainer (event_id, person_id)
    select item.id, person.id from person where legacy_user_id = schedule.r_trener;

    if lesson.ri_partner is null then
      select * into par from pary where p_id = lesson.ri_partner;
      if par.p_id_partnerka is not null then
        insert into event_registration (event_id, person_id, is_confirmed)
        select item.id, person.id, true from person where legacy_user_id = par.p_id_partner;
      else
        insert into event_registration (event_id, couple_id, is_confirmed)
        select item.id, couple.id, true from couple where legacy_pary_id = lesson.ri_partner;
      end if;
    end if;
  end loop;
end;
$$;

-- nabidka
-- event type=nabidka
-- event_trainer
-- nabidka_item
-- event_registration.couple OR person_id, payment=null
-- event_lesson_demand => trainer, count
