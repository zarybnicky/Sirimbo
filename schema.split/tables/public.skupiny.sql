CREATE TABLE public.skupiny (
    s_id bigint NOT NULL,
    s_name text NOT NULL,
    s_description text NOT NULL,
    s_color_rgb text NOT NULL,
    s_color_text text DEFAULT ''::text NOT NULL,
    s_location text DEFAULT ''::text NOT NULL,
    s_visible boolean DEFAULT true NOT NULL,
    ordering integer DEFAULT 1 NOT NULL,
    internal_info text DEFAULT '[]'::jsonb NOT NULL,
    cohort_group bigint,
    id bigint GENERATED ALWAYS AS (s_id) STORED,
    tenant_id bigint DEFAULT public.current_tenant_id() NOT NULL
);

GRANT ALL ON TABLE public.skupiny TO anonymous;
ALTER TABLE public.skupiny ENABLE ROW LEVEL SECURITY;

ALTER TABLE ONLY public.skupiny
    ADD CONSTRAINT idx_23934_primary PRIMARY KEY (s_id);
ALTER TABLE ONLY public.skupiny
    ADD CONSTRAINT skupiny_cohort_group_fkey FOREIGN KEY (cohort_group) REFERENCES public.cohort_group(id);

CREATE POLICY admin_all ON public.skupiny TO administrator USING (true) WITH CHECK (true);
CREATE POLICY all_view ON public.skupiny FOR SELECT USING (true);
CREATE POLICY my_tenant ON public.skupiny AS RESTRICTIVE USING ((tenant_id = public.current_tenant_id())) WITH CHECK ((tenant_id = public.current_tenant_id()));

CREATE INDEX s_visible ON public.skupiny USING btree (s_visible);
CREATE INDEX skupiny_cohort_group_idx ON public.skupiny USING btree (cohort_group);
CREATE INDEX skupiny_ordering_idx ON public.skupiny USING btree (ordering);