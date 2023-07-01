CREATE TABLE app_private.video_list (
    vl_id bigint NOT NULL,
    vl_url text NOT NULL,
    vl_title text NOT NULL,
    vl_description text NOT NULL,
    vl_count bigint NOT NULL,
    vl_created_at timestamp with time zone NOT NULL,
    vl_last_checked timestamp with time zone DEFAULT CURRENT_TIMESTAMP
);

GRANT ALL ON TABLE app_private.video_list TO anonymous;
ALTER TABLE app_private.video_list ENABLE ROW LEVEL SECURITY;

ALTER TABLE ONLY app_private.video_list
    ADD CONSTRAINT idx_24009_primary PRIMARY KEY (vl_id);

CREATE POLICY admin_all ON app_private.video_list TO administrator USING (true) WITH CHECK (true);
CREATE POLICY all_view ON app_private.video_list FOR SELECT USING (true);

