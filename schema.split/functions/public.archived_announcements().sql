CREATE FUNCTION public.archived_announcements() RETURNS SETOF public.upozorneni
    LANGUAGE sql STABLE
    AS $$
  select upozorneni.* from upozorneni
  where is_visible = false
    or (scheduled_until is null or scheduled_until >= now())
  order by up_timestamp_add desc;
$$;

GRANT ALL ON FUNCTION public.archived_announcements() TO anonymous;


