do $$
begin
  if not exists (
    select 1 from pg_type t join pg_namespace n on n.oid = t.typnamespace
    where n.nspname = 'public' and t.typname = 'security_event_kind'
  ) then
    create type security_event_kind as enum (
      'login_succeeded', 'login_failed', 'registration', 'impersonation',
      'password_reset_requested', 'password_changed', 'invitation_accepted',
      'person_linked', 'person_unlinked',
      'membership_granted', 'membership_revoked',
      'trainer_granted', 'trainer_revoked',
      'administrator_granted', 'administrator_revoked',
      'access_credential_issued', 'access_credential_ended'
    );
  end if;

  if not exists (
    select 1 from pg_type t join pg_namespace n on n.oid = t.typnamespace
    where n.nspname = 'public' and t.typname = 'security_event_method'
  ) then
    create type security_event_method as enum ('password', 'otp', 'manual', 'scheduled');
  end if;
end;
$$;

alter table security_event drop constraint if exists security_event_method_check;
alter table security_event
  alter column kind type security_event_kind using kind::security_event_kind,
  alter column method type security_event_method using method::security_event_method;

--!include functions/security_events.sql
