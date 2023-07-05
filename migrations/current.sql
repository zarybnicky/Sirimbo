do $$
begin
  if not exists (select * from pg_type where typcategory='E' and typname = 'event_type') then
    create type event_type as enum (
      'camp',
      'lesson',
      'reservation'
    );
  end if;
end
$$;

alter table event add column if not exists "type" event_type not null default 'camp';

create or replace VIEW public.akce AS
 SELECT event.id AS a_id,
    event.name AS a_jmeno,
    event.location_text AS a_kde,
    event.description AS a_info,
    event.since AS a_od,
    event.until AS a_do,
    event.capacity AS a_kapacita,
    event.files_legacy AS a_dokumenty,
    event.updated_at AS a_timestamp,
    event.is_locked AS a_lock,
    event.is_visible AS a_visible,
    event.summary,
    event.is_public,
    event.enable_notes
   FROM public.event
   where type='camp';

alter table person add column if not exists user_id bigint null default null;


create or replace function app_private.tg_users__sync_to_person() returns trigger as $$
BEGIN
  IF (TG_OP = 'DELETE') THEN
    update person set user_id = null where user_id = OLD.id;
    -- do nothing, the person record is unchanged

    RETURN OLD;
  END IF;

  IF (TG_OP = 'UPDATE') THEN
    UPDATE person SET
      first_name = NEW.u_jmeno,
      last_name = NEW.u_prijmeni,
      gender = NEW.u_pohlavi
    WHERE user_id = NEW.u_id;

    IF NOT FOUND THEN RETURN NULL; END IF;

    RETURN NEW;
  END IF;

  IF (TG_OP = 'INSERT') THEN
    INSERT INTO person (user_id, first_name, last_name, gender)
    VALUES (NEW.u_id, NEW.u_jmeno, NEW.u_prijmeni, NEW.u_pohlavi);

    RETURN NEW;
  END IF;
END;
$$ language plpgsql security definer;

select * from plpgsql_check_function('app_private.tg_users__sync_to_person', 'public.users');

do $$
begin
  -- insert into person (user_id, first_name, last_name, gender) select ...
end
$$;
