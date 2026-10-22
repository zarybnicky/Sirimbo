CREATE FUNCTION public.event_share_token(i public.event) RETURNS text
    LANGUAGE sql STABLE
    AS $_$
  select share_token
  from event
  where id = $1.id
    and (pg_has_role(current_user, 'administrator', 'member')
      or (pg_has_role(current_user, 'trainer', 'member')
      and app_private.can_trainer_edit_event($1.id)))
$_$;

GRANT ALL ON FUNCTION public.event_share_token(i public.event) TO anonymous;
