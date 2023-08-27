CREATE TABLE public.form_responses (
    id bigint NOT NULL,
    type text NOT NULL,
    data jsonb NOT NULL,
    url text NOT NULL,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    updated_at timestamp with time zone DEFAULT now() NOT NULL,
    tenant_id bigint DEFAULT public.current_tenant_id() NOT NULL
);

COMMENT ON TABLE public.form_responses IS '@omit update,delete';

GRANT ALL ON TABLE public.form_responses TO anonymous;
ALTER TABLE public.form_responses ENABLE ROW LEVEL SECURITY;

ALTER TABLE ONLY public.form_responses
    ADD CONSTRAINT form_responses_pkey PRIMARY KEY (id);
ALTER TABLE ONLY public.form_responses
    ADD CONSTRAINT form_responses_tenant_id_fkey FOREIGN KEY (tenant_id) REFERENCES public.tenant(id);

CREATE POLICY admin_all ON public.form_responses TO administrator USING (true) WITH CHECK (true);
CREATE POLICY my_tenant ON public.form_responses AS RESTRICTIVE USING ((tenant_id = public.current_tenant_id())) WITH CHECK ((tenant_id = public.current_tenant_id()));

CREATE TRIGGER _100_timestamps BEFORE INSERT OR UPDATE ON public.form_responses FOR EACH ROW EXECUTE FUNCTION app_private.tg__timestamps();

CREATE INDEX idx_fr_tenant ON public.form_responses USING btree (tenant_id);
CREATE INDEX type ON public.form_responses USING btree (type);
CREATE INDEX updated_at ON public.form_responses USING btree (updated_at);