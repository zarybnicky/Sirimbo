CREATE TABLE app_private.platby_category_group (
    pcg_id bigint NOT NULL,
    pcg_id_group bigint NOT NULL,
    pcg_id_category bigint NOT NULL,
    id bigint GENERATED ALWAYS AS (pcg_id) STORED NOT NULL,
    tenant_id bigint DEFAULT public.current_tenant_id() NOT NULL
);

COMMENT ON TABLE app_private.platby_category_group IS '@omit';

GRANT ALL ON TABLE app_private.platby_category_group TO anonymous;
ALTER TABLE app_private.platby_category_group ENABLE ROW LEVEL SECURITY;

ALTER TABLE ONLY app_private.platby_category_group
    ADD CONSTRAINT idx_23868_primary PRIMARY KEY (pcg_id);
ALTER TABLE ONLY app_private.platby_category_group
    ADD CONSTRAINT platby_category_group_unique_id UNIQUE (id);
ALTER TABLE ONLY app_private.platby_category_group
    ADD CONSTRAINT platby_category_group_pcg_id_category_fkey FOREIGN KEY (pcg_id_category) REFERENCES app_private.platby_category(pc_id) ON UPDATE RESTRICT ON DELETE RESTRICT;
ALTER TABLE ONLY app_private.platby_category_group
    ADD CONSTRAINT platby_category_group_pcg_id_group_fkey FOREIGN KEY (pcg_id_group) REFERENCES app_private.platby_group(pg_id) ON UPDATE RESTRICT ON DELETE RESTRICT;
ALTER TABLE ONLY app_private.platby_category_group
    ADD CONSTRAINT platby_category_group_tenant_id_fkey FOREIGN KEY (tenant_id) REFERENCES public.tenant(id) ON DELETE CASCADE;

CREATE POLICY admin_all ON app_private.platby_category_group TO administrator USING (true);
CREATE POLICY current_tenant ON app_private.platby_category_group AS RESTRICTIVE USING ((tenant_id = ( SELECT public.current_tenant_id() AS current_tenant_id)));
CREATE POLICY member_view ON app_private.platby_category_group FOR SELECT TO member USING (true);

CREATE UNIQUE INDEX idx_23868_pcg_id_group ON app_private.platby_category_group USING btree (pcg_id_group, pcg_id_category);
CREATE INDEX idx_23868_platby_category_group_pcg_id_category_fkey ON app_private.platby_category_group USING btree (pcg_id_category);
CREATE INDEX platby_category_group_pcg_id_category_idx ON app_private.platby_category_group USING btree (pcg_id_category);
CREATE INDEX platby_category_group_tenant_id_idx ON app_private.platby_category_group USING btree (tenant_id);
