CREATE FUNCTION public.user_proxy_active(c public.user_proxy) RETURNS boolean
    LANGUAGE sql STABLE
    AS $$
  select now() <@ c.active_range;
$$;

COMMENT ON FUNCTION public.user_proxy_active(c public.user_proxy) IS '@filterable';

GRANT ALL ON FUNCTION public.user_proxy_active(c public.user_proxy) TO anonymous;


