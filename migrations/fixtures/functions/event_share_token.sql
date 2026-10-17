create or replace function event_instance_share_token(i event_instance) returns text
  language sql stable
as $$
  select share_token
  from event_instance
  where id = $1.id
    and (pg_has_role(current_user, 'administrator', 'member')
      or (pg_has_role(current_user, 'trainer', 'member')
      and app_private.can_trainer_edit_instance($1.id)))
$$;

grant execute on function event_instance_share_token to anonymous;
