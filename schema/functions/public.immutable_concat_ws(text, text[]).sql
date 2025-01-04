CREATE FUNCTION public.immutable_concat_ws(text, VARIADIC text[]) RETURNS text
    LANGUAGE internal IMMUTABLE PARALLEL SAFE
    AS $$text_concat_ws$$;
