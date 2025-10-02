CREATE FUNCTION public.archived_upozorneni() RETURNS SETOF public.upozorneni
    LANGUAGE sql STABLE
    AS $$
  select upozorneni.* from upozorneni
  where is_visible = false
    or (scheduled_until is null or scheduled_until >= now())
  order by created_at desc;
$$;

GRANT ALL ON FUNCTION public.archived_upozorneni() TO anonymous;
