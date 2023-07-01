CREATE TABLE app_private.video (
    v_id bigint NOT NULL,
    v_uri text NOT NULL,
    v_title text NOT NULL,
    v_author text NOT NULL,
    v_description text NOT NULL,
    v_playlist text,
    v_created_at timestamp with time zone DEFAULT now() NOT NULL,
    v_updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL
);

GRANT ALL ON TABLE app_private.video TO anonymous;
ALTER TABLE app_private.video ENABLE ROW LEVEL SECURITY;

ALTER TABLE ONLY app_private.video
    ADD CONSTRAINT idx_23999_primary PRIMARY KEY (v_id);

CREATE POLICY admin_all ON app_private.video TO administrator USING (true) WITH CHECK (true);
CREATE POLICY all_view ON app_private.video FOR SELECT USING (true);

