CREATE OR REPLACE FUNCTION post_without_cache(input_url TEXT, data JSONB, headers http_header[] = null)
RETURNS http_response LANGUAGE plpgsql AS $$
DECLARE
  new_response http_response;
BEGIN
  SELECT * INTO new_response FROM http(('POST', input_url, headers, 'application/json', data::text));

  RETURN new_response;
END;
$$;
select verify_function('post_without_cache');
comment on function post_without_cache is '@omit';
