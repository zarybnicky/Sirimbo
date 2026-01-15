CREATE FUNCTION app_private.tg_aktuality__author() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
BEGIN
  NEW.at_kdo = current_user_id();
  RETURN NEW;
END;
$$;
