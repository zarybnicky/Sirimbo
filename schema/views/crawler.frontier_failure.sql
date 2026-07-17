CREATE VIEW crawler.frontier_failure AS
 SELECT f.id,
    f.federation,
    f.kind,
    f.key,
    f.fetch_status,
    f.process_status,
    f.error_count,
    f.next_fetch_at,
        CASE
            WHEN ((f.fetch_status = ANY (ARRAY['error'::crawler.fetch_status, 'transient'::crawler.fetch_status])) AND (f.process_status = 'error'::crawler.process_status)) THEN GREATEST(COALESCE(jr.fetched_at, f.last_fetched_at, f.discovered_at), COALESCE(f.last_process_error_at, f.last_fetched_at, f.discovered_at))
            WHEN (f.process_status = 'error'::crawler.process_status) THEN COALESCE(f.last_process_error_at, f.last_fetched_at, f.discovered_at)
            ELSE COALESCE(jr.fetched_at, f.last_fetched_at, f.discovered_at)
        END AS failed_at,
    jr.url,
    jr.http_status,
    jr.error AS response_error,
    f.last_process_error AS process_error,
    concat_ws('+'::text,
        CASE
            WHEN (f.fetch_status = ANY (ARRAY['error'::crawler.fetch_status, 'transient'::crawler.fetch_status])) THEN 'fetch'::text
            ELSE NULL::text
        END,
        CASE
            WHEN (f.process_status = 'error'::crawler.process_status) THEN 'process'::text
            ELSE NULL::text
        END) AS failure,
    COALESCE(
        CASE
            WHEN (f.process_status = 'error'::crawler.process_status) THEN f.last_process_error
            ELSE NULL::text
        END, jr.error, f.last_process_error, ''::text) AS error_text
   FROM (crawler.frontier f
     LEFT JOIN crawler.json_response jr ON ((jr.id = f.last_response_id)))
  WHERE ((f.fetch_status = ANY (ARRAY['error'::crawler.fetch_status, 'transient'::crawler.fetch_status])) OR (f.process_status = 'error'::crawler.process_status));
