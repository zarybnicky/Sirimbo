
--!include functions/resend_person_invitation.sql

do $$
begin
  if not exists (
    select 1 from information_schema.columns
    where table_schema = 'public' and table_name = 'person_invitation' and column_name = 'last_sent_at'
  ) then
    alter table person_invitation add column last_sent_at timestamptz;
    update person_invitation set last_sent_at = created_at;
  end if;
end;
$$;
