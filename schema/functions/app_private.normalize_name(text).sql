CREATE FUNCTION app_private.normalize_name(text) RETURNS text
    LANGUAGE sql IMMUTABLE STRICT PARALLEL SAFE
    RETURN lower(public.unaccent('public.unaccent'::regdictionary, $1));

GRANT ALL ON FUNCTION app_private.normalize_name(text) TO anonymous;
