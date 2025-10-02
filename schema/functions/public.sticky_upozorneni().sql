CREATE FUNCTION public.sticky_upozorneni() RETURNS SETOF public.upozorneni
    LANGUAGE sql STABLE
    AS $$
  select upozorneni.* from upozorneni
  where is_visible = true and sticky = true
    and (scheduled_since is null or scheduled_since <= now())
    and (scheduled_until is null or scheduled_until >= now())
  order by created_at desc;
$$;

GRANT ALL ON FUNCTION public.sticky_upozorneni() TO anonymous;
