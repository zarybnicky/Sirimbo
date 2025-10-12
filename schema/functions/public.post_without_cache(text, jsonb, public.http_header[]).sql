CREATE FUNCTION public.post_without_cache(input_url text, data jsonb, headers public.http_header[] DEFAULT NULL::public.http_header[]) RETURNS public.http_response
    LANGUAGE plpgsql
    AS $$
DECLARE
  new_response http_response;
BEGIN
  SELECT * INTO new_response FROM http(('POST', input_url, headers, 'application/json', data::text));

  RETURN new_response;
END;
$$;

COMMENT ON FUNCTION public.post_without_cache(input_url text, data jsonb, headers public.http_header[]) IS '@omit';

GRANT ALL ON FUNCTION public.post_without_cache(input_url text, data jsonb, headers public.http_header[]) TO administrator;
