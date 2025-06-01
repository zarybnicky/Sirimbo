CREATE TABLE public.event_external_registration (
    id bigint NOT NULL,
    tenant_id bigint DEFAULT public.current_tenant_id() NOT NULL,
    event_id bigint NOT NULL,
    first_name text NOT NULL,
    last_name text NOT NULL,
    prefix_title text DEFAULT ''::text NOT NULL,
    suffix_title text DEFAULT ''::text NOT NULL,
    nationality text NOT NULL,
    birth_date date,
    tax_identification_number text,
    email public.citext NOT NULL,
    phone text NOT NULL,
    note text,
    created_by bigint DEFAULT public.current_user_id(),
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    updated_at timestamp with time zone DEFAULT now() NOT NULL
);

COMMENT ON TABLE public.event_external_registration IS '@omit update
@simpleCollections only';

GRANT SELECT,REFERENCES,DELETE,TRIGGER,TRUNCATE,MAINTAIN,UPDATE ON TABLE public.event_external_registration TO anonymous;
GRANT INSERT(event_id) ON TABLE public.event_external_registration TO anonymous;
GRANT INSERT(note) ON TABLE public.event_external_registration TO anonymous;
ALTER TABLE public.event_external_registration ENABLE ROW LEVEL SECURITY;

ALTER TABLE ONLY public.event_external_registration
    ADD CONSTRAINT event_external_registration_pkey PRIMARY KEY (id);
ALTER TABLE ONLY public.event_external_registration
    ADD CONSTRAINT event_external_registration_created_by_fkey FOREIGN KEY (created_by) REFERENCES public.users(id);
ALTER TABLE ONLY public.event_external_registration
    ADD CONSTRAINT event_external_registration_event_id_fkey FOREIGN KEY (event_id) REFERENCES public.event(id) ON UPDATE CASCADE ON DELETE CASCADE;
ALTER TABLE ONLY public.event_external_registration
    ADD CONSTRAINT event_external_registration_tenant_id_fkey FOREIGN KEY (tenant_id) REFERENCES public.tenant(id) ON UPDATE CASCADE ON DELETE CASCADE;

CREATE POLICY admin_all ON public.event_external_registration TO administrator USING (true);
CREATE POLICY admin_my ON public.event_external_registration TO member USING ((( SELECT public.event_is_registration_open(event.*) AS event_is_registration_open
   FROM public.event
  WHERE (event_external_registration.event_id = event.id)) AND (created_by = public.current_user_id())));
CREATE POLICY trainer_same_tenant ON public.event_external_registration TO trainer USING (app_private.can_trainer_edit_event(event_id)) WITH CHECK (true);
CREATE POLICY view_visible_event ON public.event_external_registration FOR SELECT TO member USING ((EXISTS ( SELECT 1
   FROM public.event
  WHERE (event_external_registration.event_id = event.id))));

CREATE TRIGGER _100_timestamps BEFORE INSERT OR UPDATE ON public.event_external_registration FOR EACH ROW EXECUTE FUNCTION app_private.tg__timestamps();
