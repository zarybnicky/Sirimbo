CREATE FUNCTION public.sticky_announcements(order_by_updated boolean DEFAULT false) RETURNS SETOF public.announcement
    LANGUAGE sql STABLE
    AS $$
  select announcement.* from announcement
  where is_visible = true and is_sticky = true
    and (scheduled_since is null or scheduled_since <= now())
    and (scheduled_until is null or scheduled_until >= now())
  order by
    case when order_by_updated then updated_at else created_at end desc,
    created_at desc;
$$;

GRANT ALL ON FUNCTION public.sticky_announcements(order_by_updated boolean) TO anonymous;
