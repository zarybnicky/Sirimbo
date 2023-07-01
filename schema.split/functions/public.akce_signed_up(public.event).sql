CREATE FUNCTION public.akce_signed_up(a public.event) RETURNS boolean
    LANGUAGE sql STABLE
    AS $$
  select exists (select ai_id from akce_item where ai_id_rodic=a.a_id and ai_user=current_user_id());
$$;

GRANT ALL ON FUNCTION public.akce_signed_up(a public.event) TO anonymous;


