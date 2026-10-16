CREATE VIEW crawler.frontier_summary WITH (security_invoker='true') AS
 SELECT federation,
    kind,
    (count(*))::integer AS total,
    (count(*) FILTER (WHERE (fetch_status = 'pending'::crawler.fetch_status)))::integer AS fetch_pending,
    (count(*) FILTER (WHERE (fetch_status = 'transient'::crawler.fetch_status)))::integer AS fetch_transient,
    (count(*) FILTER (WHERE (fetch_status = 'error'::crawler.fetch_status)))::integer AS fetch_error,
    (count(*) FILTER (WHERE (process_status = 'ok'::crawler.process_status)))::integer AS done,
    (count(*) FILTER (WHERE (process_status = 'error'::crawler.process_status)))::integer AS process_error,
    (count(*) FILTER (WHERE ((process_status = 'pending'::crawler.process_status) AND (fetch_status = 'ok'::crawler.fetch_status))))::integer AS process_ready,
    max(last_fetched_at) AS latest,
    (count(*) FILTER (WHERE ((next_fetch_at <= now()) AND (fetch_status = ANY (ARRAY['pending'::crawler.fetch_status, 'transient'::crawler.fetch_status])))))::integer AS fetch_due,
    (count(*) FILTER (WHERE ((next_fetch_at <= now()) AND (fetch_status = 'ok'::crawler.fetch_status) AND (process_status = 'ok'::crawler.process_status))))::integer AS refetch_due
   FROM crawler.frontier
  GROUP BY federation, kind;

COMMENT ON VIEW crawler.frontier_summary IS '@name crawler_target_summary
@behavior +list -connection -insert -update -delete';

GRANT SELECT ON TABLE crawler.frontier_summary TO system_admin;
