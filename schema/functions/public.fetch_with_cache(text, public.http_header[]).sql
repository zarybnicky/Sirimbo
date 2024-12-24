CREATE FUNCTION public.fetch_with_cache(input_url text, headers public.http_header[] DEFAULT NULL::public.http_header[]) RETURNS public.response_cache
    LANGUAGE plpgsql
    AS $$
DECLARE
  new_response record;
  cached_response response_cache;
BEGIN
  SELECT * INTO cached_response FROM response_cache WHERE url = input_url;

  IF NOT FOUND THEN
    SELECT * INTO new_response FROM http(('GET', input_url, headers, NULL, NULL));

    INSERT INTO response_cache (url, status, content, content_type)
    VALUES (input_url, new_response.status, new_response.content, new_response.content_type)
    ON CONFLICT (url) DO UPDATE
    SET status = EXCLUDED.status, content = EXCLUDED.content, content_type = EXCLUDED.content_type, cached_at = NOW()
    RETURNING * INTO cached_response;
  END IF;

  RETURN cached_response;
END;
$$;

COMMENT ON FUNCTION public.fetch_with_cache(input_url text, headers public.http_header[]) IS '@omit';
