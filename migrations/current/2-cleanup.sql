drop function if exists app_private.tg__person_email_primary;
drop function if exists app_private.tg__person_address_primary;
drop function if exists app_private.tg__person_phone_primary;
drop function if exists public.users_in_public_cohort;
drop function if exists users_date_of_oldest_payment;
drop function if exists users_date_of_newest_payment;
drop function if exists on_update_current_timestamp_rozpis;
drop function if exists on_update_current_timestamp_nabidka;

comment on function skupiny_in_current_tenant is E'@filterable
@deprecated';

alter table event_registration
    alter is_confirmed set default false,
    alter confirmed_at set default null;
drop function if exists is_current_tenant_member;

do $$
begin
  drop trigger if exists on_update_current_timestamp on upozorneni;
  drop function if exists on_update_current_timestamp_upozorneni;
  if not exists (select 1 from information_schema.columns where table_schema = 'public' and table_name = 'upozorneni' and column_name = 'created_at') then
    alter table upozorneni rename column up_timestamp_add to created_at;
    alter table upozorneni rename column up_timestamp to updated_at;
  end if;
  alter table upozorneni
      add column if not exists up_timestamp timestamptz not null generated always as (updated_at) stored,
      add column if not exists up_timestamp_add timestamptz not null generated always as (created_at) stored;
  drop trigger if exists _100_timestamps ON upozorneni;
  CREATE TRIGGER _100_timestamps BEFORE INSERT OR UPDATE ON upozorneni FOR EACH ROW EXECUTE FUNCTION app_private.tg__timestamps();

  drop trigger if exists on_update_current_timestamp on dokumenty;
  drop function if exists on_update_current_timestamp_dokumenty;
  if not exists (select 1 from information_schema.columns where table_schema = 'public' and table_name = 'dokumenty' and column_name = 'updated_at') then
    alter table dokumenty rename column d_timestamp to updated_at;
  end if;
  alter table dokumenty
      add column if not exists created_at timestamptz default current_timestamp,
      add column if not exists d_timestamp timestamptz generated always as (updated_at) stored;
  drop trigger if exists _100_timestamps ON dokumenty;
  CREATE TRIGGER _100_timestamps BEFORE INSERT OR UPDATE ON dokumenty FOR EACH ROW EXECUTE FUNCTION app_private.tg__timestamps();

  drop trigger if exists on_update_current_timestamp on aktuality;
  drop function if exists on_update_current_timestamp_aktuality;
  if not exists (select 1 from information_schema.columns where table_schema = 'public' and table_name = 'aktuality' and column_name = 'created_at') then
    alter table aktuality rename column at_timestamp_add to created_at;
    alter table aktuality rename column at_timestamp to updated_at;
  end if;
  alter table aktuality
      add column if not exists at_timestamp timestamptz not null generated always as (updated_at) stored,
      add column if not exists at_timestamp_add timestamptz not null generated always as (created_at) stored;
  drop trigger if exists _100_timestamps ON aktuality;
  CREATE TRIGGER _100_timestamps BEFORE INSERT OR UPDATE ON aktuality FOR EACH ROW EXECUTE FUNCTION app_private.tg__timestamps();

  drop trigger if exists on_update_current_timestamp on galerie_foto;
  drop function if exists on_update_current_timestamp_galerie_foto;
  if not exists (select 1 from information_schema.columns where table_schema = 'public' and table_name = 'galerie_foto' and column_name = 'updated_at') then
    alter table galerie_foto rename column gf_timestamp to updated_at;
  end if;
  alter table galerie_foto
      add column if not exists created_at timestamptz default current_timestamp,
      add column if not exists gf_timestamp timestamptz generated always as (updated_at) stored;
  drop trigger if exists _100_timestamps ON galerie_foto;
  CREATE TRIGGER _100_timestamps BEFORE INSERT OR UPDATE ON galerie_foto FOR EACH ROW EXECUTE FUNCTION app_private.tg__timestamps();

  drop trigger if exists on_update_current_timestamp on users;
  drop function if exists on_update_current_timestamp_users;
  if not exists (select 1 from information_schema.columns where table_schema = 'public' and table_name = 'users' and column_name = 'updated_at') then
    alter table users rename column u_timestamp to updated_at;
  end if;
  alter table users
      add column if not exists created_at timestamptz default current_timestamp,
      add column if not exists u_timestamp timestamptz generated always as (updated_at) stored;
  drop trigger if exists _100_timestamps ON users;
  CREATE TRIGGER _100_timestamps BEFORE INSERT OR UPDATE ON users FOR EACH ROW EXECUTE FUNCTION app_private.tg__timestamps();

  drop trigger if exists on_update_event_timestamp on event;
  drop function if exists on_update_event_timestamp;
  alter table event
      add column if not exists created_at timestamptz default current_timestamp;
  drop trigger if exists _100_timestamps ON event;
  CREATE TRIGGER _100_timestamps BEFORE INSERT OR UPDATE ON event FOR EACH ROW EXECUTE FUNCTION app_private.tg__timestamps();
end;
$$;
