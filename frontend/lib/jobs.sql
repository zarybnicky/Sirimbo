/* @name EnqueueJob */
select (graphile_worker.add_job(
  identifier => :identifier!,
  payload => :payload!::text::json,
  job_key => :jobKey
)).id;
