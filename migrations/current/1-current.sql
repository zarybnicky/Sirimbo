CREATE INDEX CONCURRENTLY IF NOT EXISTS json_response_frontier_fetched_desc_idx
  ON crawler.json_response (frontier_id, fetched_at DESC);

CREATE INDEX CONCURRENTLY IF NOT EXISTS html_response_frontier_fetched_desc_idx
  ON crawler.html_response (frontier_id, fetched_at DESC);
