drop function if exists event_instance_trainer_name;

create or replace function event_trainer_name(trainer event_trainer)
  returns text
  language sql stable
begin atomic
  select concat_ws(' ', p.prefix_title, p.first_name, p.last_name)
    || case p.suffix_title when '' then '' else ', ' || p.suffix_title end
  from public.person p
  where p.id = trainer.person_id;
end;

grant execute on function event_trainer_name to anonymous;
