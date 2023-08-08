CREATE TABLE public.couple (
    id bigint NOT NULL,
    man_id bigint NOT NULL,
    woman_id bigint NOT NULL,
    since timestamp with time zone DEFAULT now() NOT NULL,
    until timestamp with time zone,
    active boolean DEFAULT true NOT NULL,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    updated_at timestamp with time zone DEFAULT now() NOT NULL,
    legacy_pary_id bigint
);

COMMENT ON TABLE public.couple IS '@omit update,delete';

GRANT ALL ON TABLE public.couple TO anonymous;
ALTER TABLE public.couple ENABLE ROW LEVEL SECURITY;

ALTER TABLE ONLY public.couple
    ADD CONSTRAINT couple_pkey PRIMARY KEY (id);
ALTER TABLE ONLY public.couple
    ADD CONSTRAINT couple_man_id_fkey FOREIGN KEY (man_id) REFERENCES public.person(id) ON UPDATE CASCADE ON DELETE CASCADE;
ALTER TABLE ONLY public.couple
    ADD CONSTRAINT couple_woman_id_fkey FOREIGN KEY (woman_id) REFERENCES public.person(id) ON UPDATE CASCADE ON DELETE CASCADE;

CREATE POLICY admin_all ON public.couple TO administrator USING (true);
CREATE POLICY view_visible_person ON public.couple FOR SELECT USING ((EXISTS ( SELECT 1
   FROM public.person
  WHERE ((couple.man_id = person.id) OR (couple.woman_id = person.id)))));

CREATE TRIGGER _100_timestamps BEFORE INSERT OR UPDATE ON public.couple FOR EACH ROW EXECUTE FUNCTION app_private.tg__timestamps();

CREATE INDEX couple_active_idx ON public.couple USING btree (active);
CREATE INDEX couple_man_id_idx ON public.couple USING btree (man_id);
CREATE INDEX couple_woman_id_idx ON public.couple USING btree (woman_id);