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
     LEFT JOIN LATERAL ( SELECT jr_1.id,
            jr_1.frontier_id,
            jr_1.url,
            jr_1.fetched_at,
            jr_1.http_status,
            jr_1.error,
            jr_1.content_hash
           FROM crawler.json_response jr_1
          WHERE (jr_1.frontier_id = f.id)
          ORDER BY jr_1.fetched_at DESC
         LIMIT 1) jr ON (true))
  WHERE ((f.fetch_status = ANY (ARRAY['error'::crawler.fetch_status, 'transient'::crawler.fetch_status])) OR (f.process_status = 'error'::crawler.process_status));
