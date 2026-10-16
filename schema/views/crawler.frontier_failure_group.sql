CREATE VIEW crawler.frontier_failure_group WITH (security_invoker='true') AS
 SELECT federation,
    kind,
    failure,
    http_status,
    "left"(regexp_replace(COALESCE(NULLIF(error_text, ''::text), '-'::text), '\s+'::text, ' '::text, 'g'::text), 180) AS error_fingerprint,
    (count(*))::integer AS count,
    max(failed_at) AS failed_at
   FROM crawler.frontier_failure
  GROUP BY federation, kind, failure, http_status, ("left"(regexp_replace(COALESCE(NULLIF(error_text, ''::text), '-'::text), '\s+'::text, ' '::text, 'g'::text), 180));

COMMENT ON VIEW crawler.frontier_failure_group IS '@name crawler_target_failure_group
@behavior +list -connection -insert -update -delete';

GRANT SELECT ON TABLE crawler.frontier_failure_group TO system_admin;
