CREATE TABLE app_private.parameters (
    pa_name character varying(40) NOT NULL,
    pa_value text NOT NULL
);

COMMENT ON TABLE app_private.parameters IS '@omit create,update,delete';

GRANT ALL ON TABLE app_private.parameters TO anonymous;
ALTER TABLE app_private.parameters ENABLE ROW LEVEL SECURITY;

ALTER TABLE ONLY app_private.parameters
    ADD CONSTRAINT idx_23816_primary PRIMARY KEY (pa_name);

CREATE POLICY admin_all ON app_private.parameters TO administrator USING (true) WITH CHECK (true);
CREATE POLICY all_view ON app_private.parameters FOR SELECT USING (true);

