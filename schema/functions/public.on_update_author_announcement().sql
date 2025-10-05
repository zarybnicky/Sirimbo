CREATE FUNCTION public.on_update_author_announcement() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
begin
  if new.author_id is null then
    new.author_id = current_user_id();
  end if;
  return new;
end;
$$;
