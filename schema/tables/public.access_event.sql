CREATE TABLE public.access_event (
    id bigint NOT NULL,
    tenant_id bigint DEFAULT public.current_tenant_id() NOT NULL,
    external_id text NOT NULL,
    device text NOT NULL,
    kind public.access_credential_kind NOT NULL,
    code text NOT NULL,
    person_id bigint,
    occurred_at timestamp with time zone NOT NULL,
    received_at timestamp with time zone DEFAULT now() NOT NULL,
    allowed boolean NOT NULL,
    reason text DEFAULT ''::text NOT NULL,
    CONSTRAINT access_event_code_check CHECK (((code <> ''::text) AND (code = btrim(code)))),
    CONSTRAINT access_event_device_check CHECK (((device <> ''::text) AND (device = btrim(device)))),
    CONSTRAINT access_event_external_id_check CHECK (((external_id <> ''::text) AND (external_id = btrim(external_id))))
);

COMMENT ON TABLE public.access_event IS '@omit create,update,delete
@simpleCollections only';

GRANT ALL ON TABLE public.access_event TO anonymous;
ALTER TABLE public.access_event ENABLE ROW LEVEL SECURITY;

ALTER TABLE ONLY public.access_event
    ADD CONSTRAINT access_event_pkey PRIMARY KEY (id);
ALTER TABLE ONLY public.access_event
    ADD CONSTRAINT access_event_tenant_id_external_id_key UNIQUE (tenant_id, external_id);
ALTER TABLE ONLY public.access_event
    ADD CONSTRAINT access_event_person_id_fkey FOREIGN KEY (person_id) REFERENCES public.person(id);
ALTER TABLE ONLY public.access_event
    ADD CONSTRAINT access_event_tenant_id_fkey FOREIGN KEY (tenant_id) REFERENCES public.tenant(id);

CREATE POLICY admin_view ON public.access_event FOR SELECT TO administrator USING (true);
CREATE POLICY current_tenant ON public.access_event AS RESTRICTIVE USING ((tenant_id = public.current_tenant_id()));
CREATE POLICY my_view ON public.access_event FOR SELECT USING ((person_id = ANY (public.current_person_ids())));

CREATE INDEX access_event_credential_idx ON public.access_event USING btree (tenant_id, kind, code, occurred_at DESC);
CREATE INDEX access_event_person_idx ON public.access_event USING btree (tenant_id, person_id, occurred_at DESC);
