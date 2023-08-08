CREATE TABLE public.attendee_external (
    id bigint NOT NULL,
    event_id bigint NOT NULL,
    first_name text NOT NULL,
    last_name text NOT NULL,
    email text NOT NULL,
    phone text NOT NULL,
    notes text DEFAULT ''::text NOT NULL,
    birth_number text,
    guardian_name text DEFAULT ''::text NOT NULL,
    managed_by bigint,
    confirmed_by bigint,
    confirmed_at timestamp with time zone,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    updated_at timestamp with time zone DEFAULT now() NOT NULL,
    tenant_id bigint DEFAULT public.current_tenant_id() NOT NULL
);

COMMENT ON TABLE public.attendee_external IS '@omit';

GRANT ALL ON TABLE public.attendee_external TO anonymous;
ALTER TABLE public.attendee_external ENABLE ROW LEVEL SECURITY;

ALTER TABLE ONLY public.attendee_external
    ADD CONSTRAINT attendee_external_pkey PRIMARY KEY (id);
ALTER TABLE ONLY public.attendee_external
    ADD CONSTRAINT attendee_external_confirmed_by_fkey FOREIGN KEY (confirmed_by) REFERENCES public.users(u_id);
ALTER TABLE ONLY public.attendee_external
    ADD CONSTRAINT attendee_external_event_id_fkey FOREIGN KEY (event_id) REFERENCES public.event(id);
ALTER TABLE ONLY public.attendee_external
    ADD CONSTRAINT attendee_external_managed_by_fkey FOREIGN KEY (managed_by) REFERENCES public.users(u_id);

CREATE POLICY admin_all ON public.attendee_external TO administrator USING (true) WITH CHECK (true);
CREATE POLICY my_tenant ON public.attendee_external AS RESTRICTIVE USING ((tenant_id = public.current_tenant_id())) WITH CHECK ((tenant_id = public.current_tenant_id()));
CREATE POLICY select_member ON public.attendee_external FOR SELECT TO member USING (true);

CREATE INDEX confirmed_by ON public.attendee_external USING btree (confirmed_by);
CREATE INDEX event_id ON public.attendee_external USING btree (event_id);
CREATE INDEX managed_by ON public.attendee_external USING btree (managed_by);