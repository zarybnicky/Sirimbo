CREATE FUNCTION public.invitation_name(token uuid) RETURNS text
    LANGUAGE sql STABLE SECURITY DEFINER
    SET search_path TO 'pg_catalog', 'public', 'pg_temp'
    AS $$
select person.name
from person_invitation join person on person.id=person_id
where access_token=token and used_at is null;
$$;

GRANT ALL ON FUNCTION public.invitation_name(token uuid) TO anonymous;
