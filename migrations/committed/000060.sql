--! Previous: sha1:7dfb5c476b3c145cb90470579d8488b9590c25c1
--! Hash: sha1:9f0c754ee344787aecc1e9cef3f37b432f64dc08

--! split: 1-current.sql
drop function if exists public.queue_announcement_notifications;

drop table if exists upozorneni;
drop table if exists upozorneni_skupiny;

alter table users drop column if exists u_timestamp;
alter table aktuality drop column if exists at_timestamp;
alter table aktuality drop column if exists at_timestamp_add;
alter table galerie_foto drop column if exists gf_timestamp;

COMMENT ON CONSTRAINT aktuality_at_foto_main_fkey ON public.aktuality IS '';
comment on constraint dokumenty_d_kdo_fkey on dokumenty is '';

do $$ begin
  if exists (select 1 from information_schema.columns where table_name = 'users' and column_name = 'u_id') then
    ALTER TABLE ONLY public.event_external_registration
      drop CONSTRAINT event_external_registration_created_by_fkey;

    ALTER TABLE users drop column id;
    alter table users rename column u_id to id;

    ALTER TABLE ONLY event_external_registration
      ADD CONSTRAINT event_external_registration_created_by_fkey FOREIGN KEY (created_by) REFERENCES users(id);
  end if;

  if exists (select 1 from information_schema.columns where table_name = 'aktuality' and column_name = 'at_id') then
    ALTER TABLE aktuality drop column id;
    alter table aktuality rename column at_id to id;
  end if;

  if exists (select 1 from information_schema.columns where table_name = 'galerie_foto' and column_name = 'gf_id') then
    ALTER TABLE galerie_foto drop column id;
    alter table galerie_foto rename column gf_id to id;
  end if;

  if exists (select 1 from information_schema.columns where table_name = 'galerie_dir' and column_name = 'gd_id') then
    ALTER TABLE galerie_dir drop column id;
    alter table galerie_dir rename column gd_id to id;
  end if;

  if exists (select 1 from information_schema.columns where table_name = 'dokumenty' and column_name = 'd_id') then
    ALTER TABLE dokumenty drop column id;
    alter table dokumenty rename column d_id to id;
  end if;
end $$;

create or replace function sticky_announcements(
  order_by_updated boolean default false
) returns setof announcement
language sql stable
as $$
  with audience_claims as (
    select
      translate(coalesce(nullif(current_setting('jwt.claims.my_cohort_ids', true), ''), '[]'), '[]', '{}')::bigint[] as cohort_ids,
      coalesce(nullif(current_setting('jwt.claims.is_member', true), '')::boolean, false) as is_member,
      coalesce(nullif(current_setting('jwt.claims.is_trainer', true), '')::boolean, false) as is_trainer,
      coalesce(nullif(current_setting('jwt.claims.is_admin', true), '')::boolean, false) as is_admin
  )
  select announcement.*
  from announcement
  cross join audience_claims ac
  where is_visible = true
    and is_sticky = true
    and (scheduled_since is null or scheduled_since <= now())
    and (scheduled_until is null or scheduled_until >= now())
    and (
      not exists (
        select 1
        from announcement_audience aa_all
        where aa_all.announcement_id = announcement.id
      )
      or exists (
        select 1
        from announcement_audience aa
        where aa.announcement_id = announcement.id
          and (
            (aa.cohort_id is not null and aa.cohort_id = any (ac.cohort_ids))
            or (aa.audience_role = 'member' and ac.is_member)
            or (aa.audience_role = 'trainer' and ac.is_trainer)
            or (aa.audience_role = 'administrator' and ac.is_admin)
          )
      )
    )
  order by
    case when order_by_updated then updated_at else created_at end desc,
    created_at desc;
$$;
grant all on function sticky_announcements(order_by_updated boolean) to anonymous;

create or replace function my_announcements(
  archive boolean default false,
  order_by_updated boolean default false
) returns setof announcement
language sql stable
as $$
  with audience_claims as (
    select
      translate(coalesce(nullif(current_setting('jwt.claims.my_cohort_ids', true), ''), '[]'), '[]', '{}')::bigint[] as cohort_ids,
      coalesce(nullif(current_setting('jwt.claims.is_member', true), '')::boolean, false) as is_member,
      coalesce(nullif(current_setting('jwt.claims.is_trainer', true), '')::boolean, false) as is_trainer,
      coalesce(nullif(current_setting('jwt.claims.is_admin', true), '')::boolean, false) as is_admin
  )
  select announcement.*
  from announcement
  cross join audience_claims ac
  where is_visible = not archive
    and is_sticky = false
    and (scheduled_since is null or scheduled_since <= now())
    and (scheduled_until is null or scheduled_until >= now())
    and (
      not exists (
        select 1
        from announcement_audience aa_all
        where aa_all.announcement_id = announcement.id
      )
      or exists (
        select 1
        from announcement_audience aa
        where aa.announcement_id = announcement.id
          and (
            (aa.cohort_id is not null and aa.cohort_id = any (ac.cohort_ids))
            or (aa.audience_role = 'member' and ac.is_member)
            or (aa.audience_role = 'trainer' and ac.is_trainer)
            or (aa.audience_role = 'administrator' and ac.is_admin)
          )
      )
    )
  order by
    case when order_by_updated then updated_at else created_at end desc,
    created_at desc;
$$;
grant all on function my_announcements(archive boolean, order_by_updated boolean) to anonymous;
