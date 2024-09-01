CREATE FUNCTION public.person_name(p public.person) RETURNS text
    LANGUAGE sql STABLE
    AS $$
  select concat_ws(' ', p.prefix_title, p.first_name, p.last_name) || (case p.suffix_title when '' then '' else ', ' || p.suffix_title end);
$$;

COMMENT ON FUNCTION public.person_name(p public.person) IS '@omit';

GRANT ALL ON FUNCTION public.person_name(p public.person) TO anonymous;


