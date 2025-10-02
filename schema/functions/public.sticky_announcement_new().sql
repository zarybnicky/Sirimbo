CREATE FUNCTION public.sticky_announcement_new() RETURNS SETOF public.announcement
    LANGUAGE sql STABLE
    AS $$
  select announcement.* from announcement
  where is_visible = true and is_sticky = true
    and (scheduled_since is null or scheduled_since <= now())
    and (scheduled_until is null or scheduled_until >= now())
  order by created_at desc;
$$;

GRANT ALL ON FUNCTION public.sticky_announcement_new() TO anonymous;
