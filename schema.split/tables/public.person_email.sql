CREATE TABLE public.person_email (
    person_id bigint NOT NULL,
    email text NOT NULL,
    is_primary boolean DEFAULT false,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    updated_at timestamp with time zone DEFAULT now() NOT NULL
);

COMMENT ON TABLE public.person_email IS '@omit create,update,delete
@simpleCollections only';

GRANT ALL ON TABLE public.person_email TO anonymous;
ALTER TABLE public.person_email ENABLE ROW LEVEL SECURITY;

ALTER TABLE ONLY public.person_email
    ADD CONSTRAINT person_email_pkey PRIMARY KEY (person_id, email);
ALTER TABLE ONLY public.person_email
    ADD CONSTRAINT person_email_person_id_fkey FOREIGN KEY (person_id) REFERENCES public.person(id);

CREATE POLICY admin_all ON public.person_email TO administrator USING (true);
CREATE POLICY admin_personal ON public.person_email USING ((person_id IN ( SELECT public.my_person_ids() AS my_person_ids)));
CREATE POLICY view_visible_person ON public.person_email FOR SELECT USING ((EXISTS ( SELECT 1
   FROM public.person
  WHERE (person_email.person_id = person.id))));

CREATE TRIGGER _100_timestamps BEFORE INSERT OR UPDATE ON public.person_email FOR EACH ROW EXECUTE FUNCTION app_private.tg__timestamps();
CREATE TRIGGER _200_primary BEFORE INSERT OR UPDATE ON public.person_email FOR EACH ROW EXECUTE FUNCTION app_private.tg__person_email_primary();

CREATE INDEX person_email_person_id_idx ON public.person_email USING btree (person_id);