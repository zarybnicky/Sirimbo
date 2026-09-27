CREATE FUNCTION app_private.normalize_name(text) RETURNS text
    LANGUAGE sql IMMUTABLE STRICT PARALLEL SAFE
    AS $_$
  select lower(public.unaccent('public.unaccent', $1));
$_$;

GRANT ALL ON FUNCTION app_private.normalize_name(text) TO anonymous;
