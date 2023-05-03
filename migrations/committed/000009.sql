--! Previous: sha1:85b3d021e236aa96ea55fd5da915dfaaf3b01051
--! Hash: sha1:6aa507ca2df6c09b5682785f95f381d946bf3855

-- Write your migration here
drop policy if exists insert_all on attendee_external;

drop function if exists create_participation_external;
create or replace function create_participation_external(
       event_id bigint,
       first_name text,
       last_name text,
       guardian_name text,
       email text,
       phone text,
       notes text,
       birth_number text
) returns void as $$
declare
  event akce;
begin
  select * into event from akce where a_id=event_id;
  if event is null then
    raise exception 'ITEM_NOT_FOUND' using errcode = '28000';
  end if;

  if event.a_lock then
    raise exception 'ITEM_LOCKED' using errcode = '42501';
  end if;

  INSERT INTO attendee_external (event_id, first_name, last_name, guardian_name, email, phone, notes, birth_number)
  values (event_id, first_name, last_name, guardian_name, email, phone, notes, birth_number);
end;
$$ language plpgsql strict volatile security definer;
select * from plpgsql_check_function('public.create_participation_external');
grant execute on function create_participation_external to anonymous;
