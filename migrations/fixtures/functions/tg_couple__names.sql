create or replace function app_private.tg_couple__names() returns trigger
language plpgsql security definer
set search_path = pg_catalog, public, pg_temp as $$
begin
  select
    concat_ws(' - ', concat_ws(' ', man.first_name, man.last_name), concat_ws(' ', woman.first_name, woman.last_name)),
    concat_ws(' - ', man.last_name, woman.last_name)
  into new.name, new.short_name
  from person man, person woman
  where man.id = new.man_id and woman.id = new.woman_id
  -- A concurrent rename must finish before we read, or refresh this couple after our write.
  for share of man, woman;
  return new;
end;
$$;

create or replace function app_private.tg_person__couple_names() returns trigger
language plpgsql security definer
set search_path = pg_catalog, public, pg_temp as $$
begin
  -- Re-run the couple trigger so formatting has one definition.
  update couple set name = name where man_id = new.id or woman_id = new.id;
  return new;
end;
$$;

drop trigger if exists _050_names on couple;
create trigger _050_names
  before insert or update of man_id, woman_id, name, short_name on couple
  for each row execute function app_private.tg_couple__names();

drop trigger if exists _500_couple_names on person;
create trigger _500_couple_names
  after update of first_name, last_name on person
  for each row when (old.first_name is distinct from new.first_name or old.last_name is distinct from new.last_name)
  execute function app_private.tg_person__couple_names();
