CREATE or replace FUNCTION archived_announcements() RETURNS SETOF announcement
    LANGUAGE sql STABLE
    AS $$
  select announcement.* from announcement
  where is_visible = false
    or (scheduled_until is null or scheduled_until >= now())
  order by created_at desc;
$$;
GRANT ALL ON FUNCTION archived_announcements TO anonymous;

CREATE or replace FUNCTION sticky_announcements(
  order_by_updated boolean default false
) RETURNS SETOF announcement
    LANGUAGE sql STABLE
    AS $$
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
GRANT ALL ON FUNCTION sticky_announcements TO anonymous;

CREATE or replace FUNCTION my_announcements(
  archive boolean DEFAULT false,
  order_by_updated boolean default false
) RETURNS SETOF announcement LANGUAGE sql STABLE AS $$
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
GRANT ALL ON FUNCTION my_announcements TO anonymous;
