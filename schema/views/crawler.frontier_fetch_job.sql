CREATE VIEW crawler.frontier_fetch_job AS
 WITH jobs AS (
         SELECT j_1.id,
            j_1.key,
            "substring"(j_1.key, '^fetch:(.*):[0-9]+$'::text) AS host,
                CASE
                    WHEN ((pj.payload ->> 'id'::text) ~ '^[0-9]+$'::text) THEN ((pj.payload ->> 'id'::text))::bigint
                    ELSE NULL::bigint
                END AS frontier_id,
            j_1.run_at,
            j_1.locked_at,
            j_1.attempts,
            j_1.max_attempts,
            j_1.last_error,
            j_1.updated_at
           FROM (graphile_worker.jobs j_1
             JOIN graphile_worker._private_jobs pj ON ((pj.id = j_1.id)))
          WHERE (j_1.task_identifier = 'frontier_fetch'::text)
        )
 SELECT j.id AS job_id,
    j.key AS job_key,
    j.host,
    j.run_at,
    j.locked_at,
    j.attempts,
    j.max_attempts,
    j.last_error AS job_error,
    j.updated_at AS job_updated_at,
    f.id AS frontier_id,
    f.federation,
    f.kind,
    f.key AS frontier_key,
    f.fetch_status,
    f.process_status,
    f.last_process_error AS process_error,
        CASE
            WHEN (j.attempts >= j.max_attempts) THEN 'failed'::text
            WHEN (j.locked_at IS NOT NULL) THEN 'locked'::text
            WHEN (j.run_at <= now()) THEN 'ready'::text
            ELSE 'delayed'::text
        END AS state
   FROM (jobs j
     JOIN crawler.frontier f ON ((f.id = j.frontier_id)))
  WHERE (j.frontier_id IS NOT NULL);
