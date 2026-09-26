CREATE TABLE public.security_event (
    id bigint NOT NULL,
    tenant_id bigint DEFAULT public.current_tenant_id() NOT NULL,
    user_id bigint,
    person_id bigint,
    actor_user_id bigint DEFAULT public.current_user_id(),
    kind text NOT NULL,
    method text NOT NULL,
    occurred_at timestamp with time zone DEFAULT now() NOT NULL,
    effective_at timestamp with time zone DEFAULT now() NOT NULL,
    CONSTRAINT security_event_method_check CHECK ((method = ANY (ARRAY['password'::text, 'otp'::text, 'manual'::text, 'scheduled'::text])))
);

COMMENT ON TABLE public.security_event IS '@omit create,update,delete
@simpleCollections only
@behavior -query:resource:list -query:resource:connection -query:resource:single';

GRANT SELECT ON TABLE public.security_event TO member;
ALTER TABLE public.security_event ENABLE ROW LEVEL SECURITY;

ALTER TABLE ONLY public.security_event
    ADD CONSTRAINT security_event_pkey PRIMARY KEY (id);
ALTER TABLE ONLY public.security_event
    ADD CONSTRAINT security_event_actor_user_id_fkey FOREIGN KEY (actor_user_id) REFERENCES public.users(id) ON DELETE SET NULL;
ALTER TABLE ONLY public.security_event
    ADD CONSTRAINT security_event_person_id_fkey FOREIGN KEY (person_id) REFERENCES public.person(id) ON DELETE SET NULL;
ALTER TABLE ONLY public.security_event
    ADD CONSTRAINT security_event_tenant_id_fkey FOREIGN KEY (tenant_id) REFERENCES public.tenant(id) ON DELETE CASCADE;
ALTER TABLE ONLY public.security_event
    ADD CONSTRAINT security_event_user_id_fkey FOREIGN KEY (user_id) REFERENCES public.users(id) ON DELETE SET NULL;

CREATE POLICY admin_view ON public.security_event FOR SELECT TO administrator USING (true);
CREATE POLICY current_tenant ON public.security_event AS RESTRICTIVE USING ((tenant_id = ( SELECT public.current_tenant_id() AS current_tenant_id)));
CREATE POLICY self_view ON public.security_event FOR SELECT TO member USING (((user_id = ( SELECT public.current_user_id() AS current_user_id)) OR (person_id = ANY (( SELECT public.current_person_ids() AS current_person_ids)::bigint[]))));

CREATE INDEX security_event_actor_user_id_idx ON public.security_event USING btree (actor_user_id);
CREATE INDEX security_event_person_occurred_at_idx ON public.security_event USING btree (person_id, occurred_at DESC);
CREATE INDEX security_event_tenant_occurred_at_idx ON public.security_event USING btree (tenant_id, occurred_at DESC);
CREATE INDEX security_event_user_occurred_at_idx ON public.security_event USING btree (user_id, occurred_at DESC);
