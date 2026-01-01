CREATE TABLE public.couple (
    id bigint NOT NULL,
    man_id bigint NOT NULL,
    woman_id bigint NOT NULL,
    since timestamp with time zone DEFAULT now() NOT NULL,
    until timestamp with time zone,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    updated_at timestamp with time zone DEFAULT now() NOT NULL,
    legacy_pary_id bigint,
    active_range tstzrange GENERATED ALWAYS AS (tstzrange(since, until, '[)'::text)) STORED NOT NULL,
    status public.relationship_status DEFAULT 'active'::public.relationship_status NOT NULL,
    CONSTRAINT couple_until_gt_since CHECK ((until > since))
);

COMMENT ON TABLE public.couple IS '@simpleCollections only';
COMMENT ON COLUMN public.couple.legacy_pary_id IS '@omit';
COMMENT ON COLUMN public.couple.active_range IS '@omit';

GRANT ALL ON TABLE public.couple TO anonymous;
ALTER TABLE public.couple ENABLE ROW LEVEL SECURITY;

ALTER TABLE ONLY public.couple
    ADD CONSTRAINT couple_no_overlap EXCLUDE USING gist (man_id WITH =, woman_id WITH =, active_range WITH &&);
ALTER TABLE ONLY public.couple
    ADD CONSTRAINT couple_pkey PRIMARY KEY (id);
ALTER TABLE ONLY public.couple
    ADD CONSTRAINT couple_man_id_fkey FOREIGN KEY (man_id) REFERENCES public.person(id) ON UPDATE CASCADE ON DELETE CASCADE;
ALTER TABLE ONLY public.couple
    ADD CONSTRAINT couple_woman_id_fkey FOREIGN KEY (woman_id) REFERENCES public.person(id) ON UPDATE CASCADE ON DELETE CASCADE;

CREATE POLICY admin_all ON public.couple TO administrator USING (true);
CREATE POLICY view_visible_person ON public.couple FOR SELECT USING (((man_id IN ( SELECT v.person_id
   FROM app_private.visible_person_ids() v(person_id))) OR (woman_id IN ( SELECT v.person_id
   FROM app_private.visible_person_ids() v(person_id)))));

CREATE TRIGGER _100_timestamps BEFORE INSERT OR UPDATE ON public.couple FOR EACH ROW EXECUTE FUNCTION app_private.tg__timestamps();
CREATE TRIGGER _200_refresh_auth_details AFTER INSERT OR DELETE OR UPDATE ON public.couple FOR EACH ROW EXECUTE FUNCTION app_private.tg_auth_details__refresh();

CREATE INDEX couple_man_active_idx ON public.couple USING btree (man_id) INCLUDE (id) WHERE (status = 'active'::public.relationship_status);
CREATE INDEX couple_man_active_lookup ON public.couple USING btree (man_id, since, until, id) INCLUDE (woman_id, status) WHERE (status = 'active'::public.relationship_status);
CREATE INDEX couple_man_id_idx ON public.couple USING btree (man_id);
CREATE INDEX couple_woman_active_idx ON public.couple USING btree (woman_id) INCLUDE (id) WHERE (status = 'active'::public.relationship_status);
CREATE INDEX couple_woman_active_lookup ON public.couple USING btree (woman_id, since, until, id) INCLUDE (man_id, status) WHERE (status = 'active'::public.relationship_status);
CREATE INDEX couple_woman_id_idx ON public.couple USING btree (woman_id);
