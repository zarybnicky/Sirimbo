CREATE TABLE public.platby_group_skupina (
    pgs_id bigint NOT NULL,
    pgs_id_skupina bigint NOT NULL,
    pgs_id_group bigint NOT NULL,
    id bigint GENERATED ALWAYS AS (pgs_id) STORED,
    tenant_id bigint DEFAULT public.current_tenant_id() NOT NULL
);

COMMENT ON TABLE public.platby_group_skupina IS '@omit create,update,delete';

GRANT ALL ON TABLE public.platby_group_skupina TO anonymous;
ALTER TABLE public.platby_group_skupina ENABLE ROW LEVEL SECURITY;

ALTER TABLE ONLY public.platby_group_skupina
    ADD CONSTRAINT idx_23885_primary PRIMARY KEY (pgs_id);
ALTER TABLE ONLY public.platby_group_skupina
    ADD CONSTRAINT platby_group_skupina_pgs_id_group_fkey FOREIGN KEY (pgs_id_group) REFERENCES public.platby_group(pg_id) ON UPDATE RESTRICT ON DELETE RESTRICT;
ALTER TABLE ONLY public.platby_group_skupina
    ADD CONSTRAINT platby_group_skupina_pgs_id_skupina_fkey FOREIGN KEY (pgs_id_skupina) REFERENCES public.skupiny(s_id) ON UPDATE RESTRICT ON DELETE RESTRICT;

CREATE POLICY admin_all ON public.platby_group_skupina TO administrator USING (true) WITH CHECK (true);
CREATE POLICY member_view ON public.platby_group_skupina FOR SELECT TO member USING (true);
CREATE POLICY my_tenant ON public.platby_group_skupina AS RESTRICTIVE USING ((tenant_id = public.current_tenant_id())) WITH CHECK ((tenant_id = public.current_tenant_id()));

CREATE UNIQUE INDEX idx_23885_pgs_id_skupina ON public.platby_group_skupina USING btree (pgs_id_skupina, pgs_id_group);
CREATE INDEX idx_23885_platby_group_skupina_pgs_id_group_fkey ON public.platby_group_skupina USING btree (pgs_id_group);
CREATE UNIQUE INDEX idx_23886_pgs_id_skupina ON public.platby_group_skupina USING btree (pgs_id_skupina, pgs_id_group);