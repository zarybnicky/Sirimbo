CREATE TABLE public.platby_group (
    pg_id bigint NOT NULL,
    pg_type numeric DEFAULT '1'::numeric NOT NULL,
    pg_name text NOT NULL,
    pg_description text NOT NULL,
    pg_base bigint DEFAULT '0'::bigint NOT NULL,
    id bigint GENERATED ALWAYS AS (pg_id) STORED NOT NULL,
    tenant_id bigint DEFAULT public.current_tenant_id() NOT NULL
);

COMMENT ON TABLE public.platby_group IS '@omit';

GRANT ALL ON TABLE public.platby_group TO anonymous;
ALTER TABLE public.platby_group ENABLE ROW LEVEL SECURITY;

ALTER TABLE ONLY public.platby_group
    ADD CONSTRAINT idx_23874_primary PRIMARY KEY (pg_id);
ALTER TABLE ONLY public.platby_group
    ADD CONSTRAINT platby_group_unique_id UNIQUE (id);
ALTER TABLE ONLY public.platby_group
    ADD CONSTRAINT platby_group_tenant_id_fkey FOREIGN KEY (tenant_id) REFERENCES public.tenant(id) ON DELETE CASCADE;

CREATE POLICY admin_all ON public.platby_group TO administrator USING (true);
CREATE POLICY current_tenant ON public.platby_group AS RESTRICTIVE USING ((tenant_id = ( SELECT public.current_tenant_id() AS current_tenant_id)));
CREATE POLICY member_view ON public.platby_group FOR SELECT TO member USING (true);
