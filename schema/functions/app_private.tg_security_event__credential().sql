CREATE FUNCTION app_private.tg_security_event__credential() RETURNS trigger
    LANGUAGE plpgsql SECURITY DEFINER
    SET search_path TO 'pg_catalog', 'public', 'pg_temp'
    AS $$
begin
  if tg_op = 'DELETE' then
    if old.status = 'active' then
      insert into security_event (person_id, kind, method)
      values (old.person_id, 'access_credential_ended', 'manual');
    end if;
    return old;
  end if;

  if tg_op = 'INSERT' and new.status = 'active' then
    insert into security_event (person_id, kind, method, effective_at)
    values (new.person_id, 'access_credential_issued', 'manual', new.since);
  elsif tg_op = 'UPDATE'
     and new.status is distinct from old.status
     and new.status in ('active', 'expired') then
    insert into security_event (person_id, kind, method, effective_at)
    values (
      new.person_id,
      case when new.status = 'active' then 'access_credential_issued' else 'access_credential_ended' end,
      case when current_user_id() is null then 'scheduled' else 'manual' end,
      case when new.status = 'active' then new.since else new.until end
    );
  end if;
  return new;
end;
$$;
