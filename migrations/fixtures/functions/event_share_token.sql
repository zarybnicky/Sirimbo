drop function if exists event_instance_share_token;

create or replace function event_share_token(i event) returns text
  language sql stable
as $$
  select share_token
  from event
  where id = $1.id
    and (pg_has_role(current_user, 'administrator', 'member')
      or (pg_has_role(current_user, 'trainer', 'member')
      and app_private.can_trainer_edit_event($1.id)))
$$;

grant execute on function event_share_token to anonymous;
