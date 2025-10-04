CREATE or replace FUNCTION archived_announcement() RETURNS SETOF announcement
    LANGUAGE sql STABLE
    AS $$
  select announcement.* from announcement
  where is_visible = false
    or (scheduled_until is null or scheduled_until >= now())
  order by created_at desc;
$$;
GRANT ALL ON FUNCTION archived_announcement() TO anonymous;

CREATE or replace FUNCTION sticky_announcement(
  order_by_updated boolean default false
) RETURNS SETOF announcement
    LANGUAGE sql STABLE
    AS $$
  select announcement.* from announcement
  where is_visible = true and is_sticky = true
    and (scheduled_since is null or scheduled_since <= now())
    and (scheduled_until is null or scheduled_until >= now())
  order by
    case when by_updated then updated_at else created_at end desc,
    created_at desc;
$$;
GRANT ALL ON FUNCTION sticky_announcement() TO anonymous;

CREATE or replace FUNCTION my_announcements(
  archive boolean DEFAULT false,
  order_by_updated boolean default false
) RETURNS SETOF announcement LANGUAGE sql STABLE AS $$
  select announcement.* from announcement
  where is_visible = not archive and is_sticky = false
    and (scheduled_since is null or scheduled_since <= now())
    and (scheduled_until is null or scheduled_until >= now())
  order by
    case when by_updated then updated_at else created_at end desc,
    created_at desc;
$$;
GRANT ALL ON FUNCTION my_announcements TO anonymous;
