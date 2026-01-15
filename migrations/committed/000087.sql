--! Previous: sha1:01412c05ba386c5e2b7167a4f55b941400ee7bd8
--! Hash: sha1:f6440a79c8d2a65c058f1c228b83fc9280fadab2

--! split: 1-current.sql
drop extension if exists http;
drop trigger if exists on_update_author on public.aktuality;
drop trigger if exists on_update_author_announcement on public.announcement;
drop function if exists public.on_update_author_upozorneni();
drop function if exists public.on_update_author_announcement();
drop function if exists public.on_update_author_aktuality();
drop aggregate if exists app_private.array_accum(anycompatiblearray);

alter table public.event
  drop column if exists member_price,
  drop column if exists guest_price;
alter table public.event_trainer
  drop column if exists lesson_price;
alter table public.event_instance_trainer
  drop column if exists lesson_price;

alter table public.cohort_subscription
  add column if not exists amount numeric(19,4) generated always as ((price).amount) stored,
  add column if not exists currency text generated always as (((price).currency)::text) stored;

alter table public.tenant_trainer
  add column if not exists member_price_45min_amount numeric(19,4)
    generated always as ((member_price_45min).amount) stored,
  add column if not exists member_payout_45min_amount numeric(19,4)
    generated always as ((member_payout_45min).amount) stored,
  add column if not exists guest_price_45min_amount numeric(19,4)
    generated always as ((guest_price_45min).amount) stored,
  add column if not exists guest_payout_45min_amount numeric(19,4)
    generated always as ((guest_payout_45min).amount) stored,
  add column if not exists currency text
    generated always as (((member_price_45min).currency)::text) stored;

alter table person alter name set expression as (
  public.immutable_concat_ws(
    ' ',
    VARIADIC ARRAY[
      NULLIF(btrim(prefix_title), ''),
      NULLIF(btrim(first_name), ''),
      NULLIF(btrim(last_name), ''),
      CASE
        WHEN btrim(suffix_title) = '' THEN NULL
        ELSE ', ' || btrim(suffix_title)
      END
    ]
  )
);
analyze person;

create or replace function app_private.tg_announcement__author() returns trigger
  language plpgsql
as $$
begin
  if new.author_id is null then
    new.author_id = current_user_id();
  end if;
  return new;
end;
$$;
select verify_function('app_private.tg_announcement__author', 'public.announcement');

CREATE or replace FUNCTION app_private.tg_aktuality__author() RETURNS trigger
  LANGUAGE plpgsql
AS $$
BEGIN
  NEW.at_kdo = current_user_id();
  RETURN NEW;
END;
$$;

drop trigger if exists _200_author on public.announcement;
create trigger _200_author
  before insert or update on public.announcement
  for each row execute function app_private.tg_announcement__author();

drop trigger if exists _200_author on public.aktuality;
create trigger _200_author
  before insert or update on public.aktuality
  for each row execute function app_private.tg_aktuality__author();

CREATE or replace FUNCTION public.invitation_name(token uuid) RETURNS text
  LANGUAGE sql STABLE SECURITY DEFINER
  SET search_path TO 'pg_catalog', 'public', 'pg_temp'
AS $$
select person.name
from person_invitation join person on person.id=person_id
where access_token=token and used_at is null;
$$;

drop function if exists public.person_name;
