CREATE TABLE app_private.platby_category (
    pc_id bigint NOT NULL,
    pc_name text NOT NULL,
    pc_symbol bigint NOT NULL,
    pc_amount numeric(10,2) NOT NULL,
    pc_date_due date NOT NULL,
    pc_valid_from date NOT NULL,
    pc_valid_to date NOT NULL,
    pc_use_base boolean DEFAULT false NOT NULL,
    pc_use_prefix boolean DEFAULT false NOT NULL,
    pc_archive boolean DEFAULT false NOT NULL,
    pc_visible boolean DEFAULT true NOT NULL,
    id bigint GENERATED ALWAYS AS (pc_id) STORED NOT NULL,
    tenant_id bigint DEFAULT public.current_tenant_id() NOT NULL
);

COMMENT ON TABLE app_private.platby_category IS '@omit create,update,delete';

GRANT ALL ON TABLE app_private.platby_category TO anonymous;
ALTER TABLE app_private.platby_category ENABLE ROW LEVEL SECURITY;

ALTER TABLE ONLY app_private.platby_category
    ADD CONSTRAINT idx_23855_primary PRIMARY KEY (pc_id);
ALTER TABLE ONLY app_private.platby_category
    ADD CONSTRAINT platby_category_unique_id UNIQUE (id);
ALTER TABLE ONLY app_private.platby_category
    ADD CONSTRAINT platby_category_tenant_id_fkey FOREIGN KEY (tenant_id) REFERENCES public.tenant(id) ON DELETE CASCADE;

CREATE POLICY admin_all ON app_private.platby_category TO administrator USING (true);
CREATE POLICY current_tenant ON app_private.platby_category AS RESTRICTIVE USING ((tenant_id = ( SELECT public.current_tenant_id() AS current_tenant_id)));
CREATE POLICY member_view ON app_private.platby_category FOR SELECT TO member USING (true);

CREATE UNIQUE INDEX idx_23855_pc_symbol ON app_private.platby_category USING btree (pc_symbol);
CREATE INDEX platby_category_tenant_id_idx ON app_private.platby_category USING btree (tenant_id);
