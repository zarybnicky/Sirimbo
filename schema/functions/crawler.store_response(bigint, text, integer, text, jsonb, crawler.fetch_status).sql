CREATE FUNCTION crawler.store_response(p_frontier_id bigint, p_url text, p_http_status integer, p_error text, p_content jsonb, p_fetch_status crawler.fetch_status) RETURNS bigint
    LANGUAGE plpgsql
    AS $$
declare
  previous crawler.json_response;
  observed_at timestamptz;
  payload_hash text;
  response_id bigint;
begin
  perform 1 from crawler.frontier where id = p_frontier_id for update;
  if not found then
    raise exception 'Crawler frontier % does not exist', p_frontier_id;
  end if;

  select r.* into previous
  from crawler.frontier f
  join crawler.json_response r on r.id = f.last_response_id
  where f.id = p_frontier_id;

  observed_at := clock_timestamp();
  payload_hash := encode(digest(p_content::text, 'sha256'), 'hex');

  if previous.id is not null and
    row(previous.url, previous.http_status, previous.error, previous.content_hash)
      is not distinct from row(p_url, p_http_status, p_error, payload_hash)
  then
    update crawler.json_response
    set sys_period = coalesce(sys_period, tstzrange(fetched_at, null, '[)')),
        last_observed_at = observed_at,
        observation_count = observation_count + 1
    where id = previous.id;
    response_id := previous.id;
  else
    if previous.id is not null then
      update crawler.json_response
      set sys_period = tstzrange(fetched_at, observed_at, '[)'),
          last_observed_at = coalesce(last_observed_at, fetched_at)
      where id = previous.id;
    end if;

    if p_content is not null then
      insert into crawler.json_response_cache (content) values (p_content)
      on conflict (content_hash) do nothing;
    end if;

    insert into crawler.json_response (
      frontier_id, url, http_status, error, content_hash,
      fetched_at, sys_period, last_observed_at
    ) values (
      p_frontier_id, p_url, p_http_status, p_error, payload_hash,
      observed_at, tstzrange(observed_at, null, '[)'), observed_at
    ) returning id into response_id;
  end if;

  update crawler.frontier
  set last_response_id = response_id,
      last_successful_response_id = case when p_fetch_status = 'ok'
        then response_id else last_successful_response_id end
  where id = p_frontier_id;

  return response_id;
end;
$$;

COMMENT ON FUNCTION crawler.store_response(p_frontier_id bigint, p_url text, p_http_status integer, p_error text, p_content jsonb, p_fetch_status crawler.fetch_status) IS '@behavior -*';

REVOKE ALL ON FUNCTION crawler.store_response(p_frontier_id bigint, p_url text, p_http_status integer, p_error text, p_content jsonb, p_fetch_status crawler.fetch_status) FROM PUBLIC;
