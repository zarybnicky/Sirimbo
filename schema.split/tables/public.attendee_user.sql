CREATE TABLE public.attendee_user (
    id bigint NOT NULL,
    event_id bigint NOT NULL,
    user_id bigint NOT NULL,
    birth_year smallint NOT NULL,
    notes text DEFAULT ''::text NOT NULL,
    tenant_id bigint DEFAULT public.current_tenant_id() NOT NULL
);

GRANT ALL ON TABLE public.attendee_user TO anonymous;
ALTER TABLE public.attendee_user ENABLE ROW LEVEL SECURITY;

ALTER TABLE ONLY public.attendee_user
    ADD CONSTRAINT akce_item_unique_user_event_key UNIQUE (user_id, event_id);
ALTER TABLE ONLY public.attendee_user
    ADD CONSTRAINT idx_23747_primary PRIMARY KEY (id);
ALTER TABLE ONLY public.attendee_user
    ADD CONSTRAINT akce_item_ai_id_rodic_fkey FOREIGN KEY (event_id) REFERENCES public.event(id) ON UPDATE CASCADE ON DELETE CASCADE;
ALTER TABLE ONLY public.attendee_user
    ADD CONSTRAINT akce_item_ai_user_fkey FOREIGN KEY (user_id) REFERENCES public.users(u_id) ON UPDATE RESTRICT ON DELETE RESTRICT;

CREATE POLICY admin_all ON public.attendee_user TO administrator USING (true) WITH CHECK (true);
CREATE POLICY manage_own ON public.attendee_user TO member USING ((user_id = public.current_user_id())) WITH CHECK ((user_id = public.current_user_id()));
CREATE POLICY my_tenant ON public.attendee_user AS RESTRICTIVE USING ((tenant_id = public.current_tenant_id())) WITH CHECK ((tenant_id = public.current_tenant_id()));
CREATE POLICY select_member ON public.attendee_user FOR SELECT TO member USING (true);

CREATE INDEX idx_23747_akce_item_ai_id_rodic_fkey ON public.attendee_user USING btree (event_id);
CREATE INDEX idx_23747_akce_item_ai_user_fkey ON public.attendee_user USING btree (user_id);