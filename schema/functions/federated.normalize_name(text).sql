CREATE FUNCTION federated.normalize_name(text) RETURNS text
    LANGUAGE sql IMMUTABLE STRICT PARALLEL SAFE
    AS $_$
  SELECT lower(public.unaccent('public.unaccent', $1));
$_$;
