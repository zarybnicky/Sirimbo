
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
      gender = NEW.u_pohlavi::gender_type
    WHERE user_id = NEW.u_id;

    IF NOT FOUND THEN RETURN NULL; END IF;

    RETURN NEW;
  END IF;

  IF (TG_OP = 'INSERT') THEN
    INSERT INTO person (user_id, first_name, last_name, gender)
    VALUES (NEW.u_id, NEW.u_jmeno, NEW.u_prijmeni, NEW.u_pohlavi::gender_type);

    RETURN NEW;
  END IF;

  RETURN null;
END;
$$ language plpgsql security definer;

select verify_function('app_private.tg_users__sync_to_person', 'public.users');

do $$
begin
  -- insert into person (user_id, first_name, last_name, gender) select ...
end
$$;
