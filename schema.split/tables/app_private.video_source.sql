CREATE TABLE app_private.video_source (
    vs_id bigint NOT NULL,
    vs_url text NOT NULL,
    vs_title text,
    vs_description text,
    vs_created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    vs_last_checked timestamp with time zone
);

GRANT ALL ON TABLE app_private.video_source TO anonymous;
ALTER TABLE app_private.video_source ENABLE ROW LEVEL SECURITY;

ALTER TABLE ONLY app_private.video_source
    ADD CONSTRAINT idx_24019_primary PRIMARY KEY (vs_id);

CREATE POLICY admin_all ON app_private.video_source TO administrator USING (true) WITH CHECK (true);
CREATE POLICY all_view ON app_private.video_source FOR SELECT USING (true);

