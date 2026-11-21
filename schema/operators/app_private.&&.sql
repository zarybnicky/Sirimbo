CREATE OPERATOR app_private.&& (
    FUNCTION = app_private.tstzrange_overlaps_internal,
    LEFTARG = tstzrange,
    RIGHTARG = anyrange,
    RESTRICT = matchingsel,
    JOIN = matchingjoinsel
);
