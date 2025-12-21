CREATE INDEX IF NOT EXISTS json_response_frontier_fetched_desc_idx
  ON crawler.json_response (frontier_id, fetched_at DESC);

CREATE INDEX IF NOT EXISTS html_response_frontier_fetched_desc_idx
  ON crawler.html_response (frontier_id, fetched_at DESC);

CREATE INDEX IF NOT EXISTS frontier_process_pending_ok_gone_pick_idx
  ON crawler.frontier (last_fetched_at, discovered_at, id)
  WHERE process_status = 'pending'
    AND fetch_status IN ('ok','gone');

