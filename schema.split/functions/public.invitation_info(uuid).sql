CREATE FUNCTION public.invitation_info(token uuid) RETURNS text
    LANGUAGE sql STABLE SECURITY DEFINER
    AS $$
  select person.email from person_invitation join person on person.id=person_id where access_token=token and used_at is null;
$$;

GRANT ALL ON FUNCTION public.invitation_info(token uuid) TO anonymous;


