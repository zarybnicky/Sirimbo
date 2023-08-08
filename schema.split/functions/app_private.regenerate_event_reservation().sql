CREATE FUNCTION app_private.regenerate_event_reservation() RETURNS void
    LANGUAGE plpgsql STRICT SECURITY DEFINER
    AS $$
declare
  schedule nabidka;
  lesson nabidka_item;
  item event;
  item_instance event_instance;
  par pary;
  trainer event_trainer;
  reg event_registration;
begin
  delete from event where "type" = 'reservation';
  for schedule in select * from nabidka loop
    insert into event (name, description, location_text, type, since, until, capacity, is_locked, is_visible)
    values ('', '', '', 'reservation', schedule.n_od + time '00:00', schedule.n_do + time '23:59:59.999999', schedule.n_pocet_hod, schedule.n_lock, schedule.n_visible)
    returning * into item;

    insert into event_instance (event_id, range)
    values (item.id, tstzrange(schedule.n_od + time '00:00', schedule.n_do + time '23:59:59.999999', '[]'))
    returning * into item_instance;

    insert into event_trainer (event_id, person_id)
    select item.id, person.id from person where legacy_user_id = schedule.n_trener
    returning * into trainer;

    insert into event_instance_trainer (instance_id, person_id)
    select item_instance.id, person.id from person where legacy_user_id = schedule.n_trener;

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



