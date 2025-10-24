CREATE TABLE app_private.platby_item (
    pi_id bigint NOT NULL,
    pi_id_user bigint,
    pi_id_category bigint NOT NULL,
    pi_id_raw bigint,
    pi_amount numeric(10,2) NOT NULL,
    pi_date date NOT NULL,
    pi_prefix integer DEFAULT 2000 NOT NULL,
    id bigint GENERATED ALWAYS AS (pi_id) STORED NOT NULL,
    tenant_id bigint DEFAULT public.current_tenant_id() NOT NULL,
    status public.payment_status DEFAULT 'paid'::public.payment_status NOT NULL
);

COMMENT ON TABLE app_private.platby_item IS '@omit create,update,delete';

GRANT ALL ON TABLE app_private.platby_item TO anonymous;
ALTER TABLE app_private.platby_item ENABLE ROW LEVEL SECURITY;

ALTER TABLE ONLY app_private.platby_item
    ADD CONSTRAINT idx_23891_primary PRIMARY KEY (pi_id);
ALTER TABLE ONLY app_private.platby_item
    ADD CONSTRAINT platby_item_unique_id UNIQUE (id);
ALTER TABLE ONLY app_private.platby_item
    ADD CONSTRAINT platby_item_pi_id_category_fkey FOREIGN KEY (pi_id_category) REFERENCES app_private.platby_category(pc_id) ON UPDATE RESTRICT ON DELETE RESTRICT;
ALTER TABLE ONLY app_private.platby_item
    ADD CONSTRAINT platby_item_pi_id_raw_fkey FOREIGN KEY (pi_id_raw) REFERENCES app_private.platby_raw(pr_id) ON UPDATE RESTRICT ON DELETE RESTRICT;
ALTER TABLE ONLY app_private.platby_item
    ADD CONSTRAINT platby_item_pi_id_user_fkey FOREIGN KEY (pi_id_user) REFERENCES public.users(id) ON UPDATE RESTRICT ON DELETE RESTRICT;
ALTER TABLE ONLY app_private.platby_item
    ADD CONSTRAINT platby_item_tenant_id_fkey FOREIGN KEY (tenant_id) REFERENCES public.tenant(id) ON DELETE CASCADE;

CREATE POLICY admin_all ON app_private.platby_item TO administrator USING (true);
CREATE POLICY current_tenant ON app_private.platby_item AS RESTRICTIVE USING ((tenant_id = ( SELECT public.current_tenant_id() AS current_tenant_id)));
CREATE POLICY view_my ON app_private.platby_item FOR SELECT TO member USING ((pi_id_user = public.current_user_id()));

CREATE UNIQUE INDEX idx_23891_pi_id_raw ON app_private.platby_item USING btree (pi_id_raw);
CREATE INDEX idx_23891_platby_item_pi_id_category_fkey ON app_private.platby_item USING btree (pi_id_category);
CREATE INDEX idx_23891_platby_item_pi_id_user_fkey ON app_private.platby_item USING btree (pi_id_user);
CREATE INDEX platby_item_pi_id_category_idx ON app_private.platby_item USING btree (pi_id_category);
CREATE INDEX platby_item_tenant_id_idx ON app_private.platby_item USING btree (tenant_id);
