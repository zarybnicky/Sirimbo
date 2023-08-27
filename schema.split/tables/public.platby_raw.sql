CREATE TABLE public.platby_raw (
    pr_id bigint NOT NULL,
    pr_raw bytea NOT NULL,
    pr_hash text NOT NULL,
    pr_sorted boolean DEFAULT true NOT NULL,
    pr_discarded boolean DEFAULT true NOT NULL,
    id bigint GENERATED ALWAYS AS (pr_id) STORED,
    tenant_id bigint DEFAULT public.current_tenant_id() NOT NULL
);

COMMENT ON TABLE public.platby_raw IS '@omit';

GRANT ALL ON TABLE public.platby_raw TO anonymous;
ALTER TABLE public.platby_raw ENABLE ROW LEVEL SECURITY;

ALTER TABLE ONLY public.platby_raw
    ADD CONSTRAINT idx_23898_primary PRIMARY KEY (pr_id);
ALTER TABLE ONLY public.platby_raw
    ADD CONSTRAINT platby_raw_tenant_id_fkey FOREIGN KEY (tenant_id) REFERENCES public.tenant(id);

CREATE POLICY admin_all ON public.platby_raw TO administrator USING (true) WITH CHECK (true);
CREATE POLICY member_view ON public.platby_raw FOR SELECT TO member USING ((EXISTS ( SELECT
   FROM public.platby_item
  WHERE ((platby_item.pi_id_raw = platby_raw.pr_id) AND (platby_item.pi_id_user = public.current_user_id())))));
CREATE POLICY my_tenant ON public.platby_raw AS RESTRICTIVE USING ((tenant_id = public.current_tenant_id())) WITH CHECK ((tenant_id = public.current_tenant_id()));

CREATE UNIQUE INDEX idx_23898_pr_hash ON public.platby_raw USING btree (pr_hash);
CREATE INDEX idx_pr_tenant ON public.platby_raw USING btree (tenant_id);