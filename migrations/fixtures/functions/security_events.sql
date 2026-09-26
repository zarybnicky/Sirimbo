create or replace function app_private.tg_relationship__status()
  returns trigger
  language plpgsql
as $$
begin
  new.status := app_private.relationship_status_next(
    now(), tstzrange(new.since, new.until, '[)'), new.status
  );
  return new;
end;
$$;

create or replace function app_private.tg_security_event__range()
  returns trigger
  language plpgsql security definer
  set search_path to pg_catalog, public, pg_temp
as $$
begin
  if tg_op = 'DELETE' then
    if old.status = 'active' then
      insert into security_event (person_id, kind, method)
      values (old.person_id, tg_argv[1], 'manual');
    end if;
    return old;
  end if;

  if tg_op = 'INSERT' and new.status = 'active' then
    insert into security_event (person_id, kind, method, effective_at)
    values (new.person_id, tg_argv[0], 'manual', new.since);
  elsif tg_op = 'UPDATE'
     and new.status is distinct from old.status
     and new.status in ('active', 'expired') then
    insert into security_event (person_id, kind, method, effective_at)
    values (
      new.person_id,
      case when new.status = 'active' then tg_argv[0] else tg_argv[1] end,
      case when current_user_id() is null then 'scheduled' else 'manual' end,
      case when new.status = 'active' then new.since else new.until end
    );
  end if;
  return new;
end;
$$;

create or replace function app_private.tg_security_event__credential()
  returns trigger
  language plpgsql security definer
  set search_path to pg_catalog, public, pg_temp
as $$
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

create or replace function app_private.tg_security_event__user_proxy()
  returns trigger
  language plpgsql security definer
  set search_path to pg_catalog, public, pg_temp
as $$
begin
  if tg_op = 'DELETE' then
    if old.status = 'active' then
      insert into security_event (user_id, person_id, kind, method)
      values (old.user_id, old.person_id, 'person_unlinked', 'manual');
    end if;
    return old;
  end if;

  if tg_op = 'INSERT' and new.status = 'active' then
    insert into security_event (user_id, person_id, kind, method, effective_at)
    values (new.user_id, new.person_id, 'person_linked', 'manual', new.since);
  elsif tg_op = 'UPDATE'
     and new.status is distinct from old.status
     and new.status in ('active', 'expired') then
    insert into security_event (user_id, person_id, kind, method, effective_at)
    values (
      new.user_id,
      new.person_id,
      case when new.status = 'active' then 'person_linked' else 'person_unlinked' end,
      case when current_user_id() is null then 'scheduled' else 'manual' end,
      case when new.status = 'active' then new.since else new.until end
    );
  end if;
  return new;
end;
$$;

create or replace function app_private.tg_security_event__password()
  returns trigger
  language plpgsql security definer
  set search_path to pg_catalog, public, pg_temp
as $$
begin
  insert into security_event (user_id, kind, method)
  values (new.id, 'password_changed', 'manual');
  return new;
end;
$$;

create or replace trigger _050_relationship_status
  before insert or update of since, until on user_proxy
  for each row execute function app_private.tg_relationship__status();
create or replace trigger _050_relationship_status
  before insert or update of since, until on tenant_membership
  for each row execute function app_private.tg_relationship__status();
create or replace trigger _050_relationship_status
  before insert or update of since, until on tenant_trainer
  for each row execute function app_private.tg_relationship__status();
create or replace trigger _050_relationship_status
  before insert or update of since, until on tenant_administrator
  for each row execute function app_private.tg_relationship__status();
create or replace trigger _050_relationship_status
  before insert or update of since, until on access_credential
  for each row execute function app_private.tg_relationship__status();

create or replace trigger _900_security_event
  after insert or update of status or delete on tenant_membership
  for each row execute function app_private.tg_security_event__range(
    'membership_granted', 'membership_revoked'
  );
create or replace trigger _900_security_event
  after insert or update of status or delete on tenant_trainer
  for each row execute function app_private.tg_security_event__range(
    'trainer_granted', 'trainer_revoked'
  );
create or replace trigger _900_security_event
  after insert or update of status or delete on tenant_administrator
  for each row execute function app_private.tg_security_event__range(
    'administrator_granted', 'administrator_revoked'
  );
create or replace trigger _900_security_event
  after insert or update of status or delete on access_credential
  for each row execute function app_private.tg_security_event__credential();
create or replace trigger _900_security_event
  after insert or update of status or delete on user_proxy
  for each row execute function app_private.tg_security_event__user_proxy();
create or replace trigger _900_security_event
  after update of u_pass on users
  for each row
  when (old.u_pass is distinct from new.u_pass)
  execute function app_private.tg_security_event__password();
