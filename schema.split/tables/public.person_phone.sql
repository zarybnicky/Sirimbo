CREATE TABLE public.person_phone (
    person_id bigint NOT NULL,
    phone text NOT NULL,
    is_primary boolean DEFAULT false,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    updated_at timestamp with time zone DEFAULT now() NOT NULL
);

COMMENT ON TABLE public.person_phone IS '@omit';

GRANT ALL ON TABLE public.person_phone TO anonymous;
ALTER TABLE public.person_phone ENABLE ROW LEVEL SECURITY;

ALTER TABLE ONLY public.person_phone
    ADD CONSTRAINT person_phone_pkey PRIMARY KEY (person_id, phone);
ALTER TABLE ONLY public.person_phone
    ADD CONSTRAINT person_phone_person_id_fkey FOREIGN KEY (person_id) REFERENCES public.person(id) ON UPDATE CASCADE ON DELETE CASCADE;

CREATE POLICY admin_all ON public.person_phone TO administrator USING (true);
CREATE POLICY admin_personal ON public.person_phone USING ((person_id IN ( SELECT public.my_person_ids() AS my_person_ids)));
CREATE POLICY view_visible_person ON public.person_phone FOR SELECT USING ((EXISTS ( SELECT 1
   FROM public.person
  WHERE (person_phone.person_id = person.id))));

CREATE TRIGGER _100_timestamps BEFORE INSERT OR UPDATE ON public.person_phone FOR EACH ROW EXECUTE FUNCTION app_private.tg__timestamps();
CREATE TRIGGER _200_primary BEFORE INSERT OR UPDATE ON public.person_phone FOR EACH ROW EXECUTE FUNCTION app_private.tg__person_phone_primary();

CREATE INDEX person_phone_person_id_idx ON public.person_phone USING btree (person_id);