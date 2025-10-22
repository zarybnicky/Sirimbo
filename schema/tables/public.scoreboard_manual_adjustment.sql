CREATE TABLE public.scoreboard_manual_adjustment (
    id bigint NOT NULL,
    tenant_id bigint DEFAULT public.current_tenant_id() NOT NULL,
    person_id bigint NOT NULL,
    cohort_id bigint,
    points integer NOT NULL,
    reason text,
    awarded_at date DEFAULT CURRENT_DATE NOT NULL,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    updated_at timestamp with time zone DEFAULT now() NOT NULL
);

COMMENT ON TABLE public.scoreboard_manual_adjustment IS '@simpleCollections only';

GRANT ALL ON TABLE public.scoreboard_manual_adjustment TO anonymous;
ALTER TABLE public.scoreboard_manual_adjustment ENABLE ROW LEVEL SECURITY;

ALTER TABLE ONLY public.scoreboard_manual_adjustment
    ADD CONSTRAINT scoreboard_manual_adjustment_pkey PRIMARY KEY (id);
ALTER TABLE ONLY public.scoreboard_manual_adjustment
    ADD CONSTRAINT scoreboard_manual_adjustment_cohort_id_fkey FOREIGN KEY (cohort_id) REFERENCES public.cohort(id);
ALTER TABLE ONLY public.scoreboard_manual_adjustment
    ADD CONSTRAINT scoreboard_manual_adjustment_person_id_fkey FOREIGN KEY (person_id) REFERENCES public.person(id) ON UPDATE CASCADE ON DELETE CASCADE;
ALTER TABLE ONLY public.scoreboard_manual_adjustment
    ADD CONSTRAINT scoreboard_manual_adjustment_tenant_id_fkey FOREIGN KEY (tenant_id) REFERENCES public.tenant(id);

CREATE POLICY admin_all ON public.scoreboard_manual_adjustment TO administrator USING (true);
CREATE POLICY current_tenant ON public.scoreboard_manual_adjustment AS RESTRICTIVE USING ((tenant_id = ( SELECT public.current_tenant_id() AS current_tenant_id)));
CREATE POLICY member_read ON public.scoreboard_manual_adjustment FOR SELECT USING (true);

CREATE TRIGGER _100_timestamps BEFORE INSERT OR UPDATE ON public.scoreboard_manual_adjustment FOR EACH ROW EXECUTE FUNCTION app_private.tg__timestamps();

CREATE INDEX scoreboard_manual_adjustment_cohort_id_idx ON public.scoreboard_manual_adjustment USING btree (cohort_id);
CREATE INDEX scoreboard_manual_adjustment_person_id_idx ON public.scoreboard_manual_adjustment USING btree (person_id);
CREATE INDEX scoreboard_manual_adjustment_tenant_id_idx ON public.scoreboard_manual_adjustment USING btree (tenant_id);
