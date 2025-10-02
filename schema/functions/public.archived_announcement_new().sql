CREATE FUNCTION public.archived_announcement_new() RETURNS SETOF public.announcement
    LANGUAGE sql STABLE
    AS $$
  select announcement.* from announcement
  where is_visible = false
    or (scheduled_until is null or scheduled_until >= now())
  order by created_at desc;
$$;

GRANT ALL ON FUNCTION public.archived_announcement_new() TO anonymous;
