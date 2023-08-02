CREATE FUNCTION app_private.regenerate_event_lesson() RETURNS void
    LANGUAGE plpgsql STRICT SECURITY DEFINER
    AS $$
declare
  schedule rozpis;
  lesson rozpis_item;
  item event;
  item_instance event_instance;
  par pary;
begin
  delete from event where "type" = 'lesson';
  for lesson in select * from rozpis_item loop
    select * into schedule from rozpis where r_id = lesson.ri_id_rodic;

    insert into event (name, description, type, since, until, location_text, is_locked, is_visible)
    values ('', '', 'lesson', schedule.r_datum + lesson.ri_od, schedule.r_datum + lesson.ri_do, schedule.r_kde, schedule.r_lock, schedule.r_visible)
    returning * into item;
    insert into event_instance (event_id, range)
    values (item.id, tstzrange(schedule.r_datum + lesson.ri_od, schedule.r_datum + lesson.ri_do, '[]'))
    returning * into item_instance;

    insert into event_trainer (event_id, person_id)
    select item.id, person.id from person where legacy_user_id = schedule.r_trener;
    insert into event_instance_trainer (instance_id, person_id)
    select item_instance.id, person.id from person where legacy_user_id = schedule.r_trener;

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



