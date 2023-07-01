CREATE FUNCTION public.crm_copy_to_form_responses() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
declare
  prospect app_private.crm_prospect;
begin
  select * into prospect from app_private.crm_prospect where id = NEW.prospect;
  insert into form_responses (type, url, data)
    values ('Přijď tančit!', NEW.origin, row_to_json(prospect.data), NEW.created_at);
  return NEW;
end;
$$;



