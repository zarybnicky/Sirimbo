CREATE FUNCTION app_private.tstzrange_overlaps_internal(tstzrange, anyrange) RETURNS boolean
    LANGUAGE internal IMMUTABLE STRICT LEAKPROOF PARALLEL SAFE
    AS $$range_overlaps$$;

GRANT ALL ON FUNCTION app_private.tstzrange_overlaps_internal(tstzrange, anyrange) TO anonymous;
