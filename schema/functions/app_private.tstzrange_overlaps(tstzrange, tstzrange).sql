CREATE FUNCTION app_private.tstzrange_overlaps(tstzrange, tstzrange) RETURNS boolean
    LANGUAGE sql IMMUTABLE STRICT LEAKPROOF PARALLEL SAFE
    RETURN ($1 OPERATOR(app_private.&&) $2);

GRANT ALL ON FUNCTION app_private.tstzrange_overlaps(tstzrange, tstzrange) TO anonymous;
