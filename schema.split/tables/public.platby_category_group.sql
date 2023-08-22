CREATE TABLE public.platby_category_group (
    pcg_id bigint NOT NULL,
    pcg_id_group bigint NOT NULL,
    pcg_id_category bigint NOT NULL,
    id bigint GENERATED ALWAYS AS (pcg_id) STORED,
    tenant_id bigint DEFAULT public.current_tenant_id() NOT NULL
);

COMMENT ON TABLE public.platby_category_group IS '@omit';

GRANT ALL ON TABLE public.platby_category_group TO anonymous;
ALTER TABLE public.platby_category_group ENABLE ROW LEVEL SECURITY;

ALTER TABLE ONLY public.platby_category_group
    ADD CONSTRAINT idx_23868_primary PRIMARY KEY (pcg_id);
ALTER TABLE ONLY public.platby_category_group
    ADD CONSTRAINT platby_category_group_pcg_id_category_fkey FOREIGN KEY (pcg_id_category) REFERENCES public.platby_category(pc_id) ON UPDATE RESTRICT ON DELETE RESTRICT;
ALTER TABLE ONLY public.platby_category_group
    ADD CONSTRAINT platby_category_group_pcg_id_group_fkey FOREIGN KEY (pcg_id_group) REFERENCES public.platby_group(pg_id) ON UPDATE RESTRICT ON DELETE RESTRICT;

CREATE POLICY admin_all ON public.platby_category_group TO administrator USING (true) WITH CHECK (true);
CREATE POLICY member_view ON public.platby_category_group FOR SELECT TO member USING (true);
CREATE POLICY my_tenant ON public.platby_category_group AS RESTRICTIVE USING ((tenant_id = public.current_tenant_id())) WITH CHECK ((tenant_id = public.current_tenant_id()));

CREATE UNIQUE INDEX idx_23868_pcg_id_group ON public.platby_category_group USING btree (pcg_id_group, pcg_id_category);
CREATE INDEX idx_23868_platby_category_group_pcg_id_category_fkey ON public.platby_category_group USING btree (pcg_id_category);