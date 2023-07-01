CREATE FUNCTION public.akce_my_notes(a public.event) RETURNS text
    LANGUAGE sql STABLE
    AS $$
  select notes from akce_item where ai_id_rodic=a.a_id and ai_user=current_user_id();
$$;

GRANT ALL ON FUNCTION public.akce_my_notes(a public.event) TO anonymous;


