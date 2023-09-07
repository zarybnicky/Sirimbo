CREATE TABLE public.person_invitation (
    id bigint NOT NULL,
    access_token uuid DEFAULT gen_random_uuid() NOT NULL,
    person_id bigint,
    tenant_id bigint DEFAULT public.current_tenant_id() NOT NULL,
    used_at timestamp with time zone,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    updated_at timestamp with time zone DEFAULT now() NOT NULL,
    email public.citext NOT NULL
);

COMMENT ON TABLE public.person_invitation IS '@omit update
@simpleCollections only';

GRANT ALL ON TABLE public.person_invitation TO anonymous;
ALTER TABLE public.person_invitation ENABLE ROW LEVEL SECURITY;

ALTER TABLE ONLY public.person_invitation
    ADD CONSTRAINT person_invitation_access_token_key UNIQUE (access_token);
ALTER TABLE ONLY public.person_invitation
    ADD CONSTRAINT person_invitation_pkey PRIMARY KEY (id);
ALTER TABLE ONLY public.person_invitation
    ADD CONSTRAINT person_invitation_person_id_fkey FOREIGN KEY (person_id) REFERENCES public.person(id) ON DELETE CASCADE;
ALTER TABLE ONLY public.person_invitation
    ADD CONSTRAINT person_invitation_tenant_id_fkey FOREIGN KEY (tenant_id) REFERENCES public.tenant(id) ON DELETE CASCADE;

CREATE POLICY admin_all ON public.person_invitation TO administrator USING (true);
CREATE POLICY admin_mine ON public.person_invitation USING ((person_id IN ( SELECT public.my_person_ids() AS my_person_ids)));

CREATE TRIGGER _500_send AFTER INSERT ON public.person_invitation FOR EACH ROW EXECUTE FUNCTION app_private.tg_person_invitation__send();

CREATE INDEX idx_pei_tenant ON public.person_invitation USING btree (tenant_id);
CREATE INDEX person_invitation_person_id_idx ON public.person_invitation USING btree (person_id);