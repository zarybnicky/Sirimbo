DROP INDEX IF EXISTS crawler.frontier_process_pending_ok_pick_idx;
CREATE INDEX frontier_process_pending_ok_pick_idx ON crawler.frontier USING btree (discovered_at, last_fetched_at, id) WHERE ((process_status = 'pending'::crawler.process_status) AND (fetch_status = 'ok'::crawler.fetch_status));
