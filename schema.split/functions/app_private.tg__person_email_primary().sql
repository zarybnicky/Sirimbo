CREATE FUNCTION app_private.tg__person_email_primary() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
begin
  if not exists (select * from person_email where person_email.person_id = person_id) then
    NEW.is_primary = true;
  end if;
  return new;
end;
$$;



