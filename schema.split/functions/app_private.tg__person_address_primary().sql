CREATE FUNCTION app_private.tg__person_address_primary() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
begin
  if not exists (select * from person_address where person_address.person_id = person_id) then
    NEW.is_primary = true;
  end if;
  return new;
end;
$$;



