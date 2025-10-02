CREATE FUNCTION public.my_announcement_new(archive boolean DEFAULT false) RETURNS SETOF public.announcement
    LANGUAGE sql STABLE
    AS $$
  select announcement.* from announcement
  where is_visible = not archive and is_sticky = false
    and (scheduled_since is null or scheduled_since <= now())
    and (scheduled_until is null or scheduled_until >= now())
  order by created_at desc;
$$;

GRANT ALL ON FUNCTION public.my_announcement_new(archive boolean) TO anonymous;
