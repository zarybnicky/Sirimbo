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
    image_id bigint,
    CONSTRAINT couple_until_gt_since CHECK ((until > since))
);

COMMENT ON TABLE public.couple IS '@behavior +list -connection -query:resource:list -query:resource:connection
@ref federationCompetitors via:public.couple_federation_link;federated.competitor
@ref federationRanklistEntries via:public.couple_federation_link;federated.competitor;federated.ranklist_entry';
COMMENT ON COLUMN public.couple.legacy_pary_id IS '@behavior -*';
COMMENT ON COLUMN public.couple.active_range IS '@behavior -*';

GRANT ALL ON TABLE public.couple TO anonymous;
ALTER TABLE public.couple ENABLE ROW LEVEL SECURITY;

ALTER TABLE ONLY public.couple
    ADD CONSTRAINT couple_no_overlap EXCLUDE USING gist (man_id WITH =, woman_id WITH =, active_range WITH &&);
ALTER TABLE ONLY public.couple
    ADD CONSTRAINT couple_pkey PRIMARY KEY (id);
ALTER TABLE ONLY public.couple
    ADD CONSTRAINT couple_image_fk FOREIGN KEY (image_id) REFERENCES public.file(id) ON DELETE SET NULL;
ALTER TABLE ONLY public.couple
    ADD CONSTRAINT couple_man_id_fkey FOREIGN KEY (man_id) REFERENCES public.person(id) ON UPDATE CASCADE ON DELETE CASCADE;
ALTER TABLE ONLY public.couple
    ADD CONSTRAINT couple_woman_id_fkey FOREIGN KEY (woman_id) REFERENCES public.person(id) ON UPDATE CASCADE ON DELETE CASCADE;

CREATE POLICY admin_all ON public.couple TO administrator USING (true);
CREATE POLICY event_share_view ON public.couple FOR SELECT TO anonymous USING ((id IN ( SELECT event_registration.couple_id
   FROM public.event_registration
  WHERE ((event_registration.registration_status = 'active'::public.event_registration_status) AND (event_registration.parent_registration_id IS NULL) AND (event_registration.couple_id IS NOT NULL) AND (event_registration.event_id = ANY ((( SELECT current_setting('jwt.claims.shared.event_ids'::text, true) AS current_setting))::bigint[]))))));
CREATE POLICY view_visible_person ON public.couple FOR SELECT USING (((man_id IN ( SELECT app_private.visible_person_ids() AS visible_person_ids)) OR (woman_id IN ( SELECT app_private.visible_person_ids() AS visible_person_ids))));

CREATE TRIGGER _100_timestamps BEFORE INSERT OR UPDATE ON public.couple FOR EACH ROW EXECUTE FUNCTION app_private.tg__timestamps();

CREATE INDEX couple_image_idx ON public.couple USING btree (image_id) WHERE (image_id IS NOT NULL);
CREATE INDEX couple_man_active_idx ON public.couple USING btree (man_id) INCLUDE (id) WHERE (status = 'active'::public.relationship_status);
CREATE INDEX couple_man_active_lookup ON public.couple USING btree (man_id, since, until, id) INCLUDE (woman_id, status) WHERE (status = 'active'::public.relationship_status);
CREATE INDEX couple_man_id_idx ON public.couple USING btree (man_id);
CREATE INDEX couple_woman_active_idx ON public.couple USING btree (woman_id) INCLUDE (id) WHERE (status = 'active'::public.relationship_status);
CREATE INDEX couple_woman_active_lookup ON public.couple USING btree (woman_id, since, until, id) INCLUDE (man_id, status) WHERE (status = 'active'::public.relationship_status);
CREATE INDEX couple_woman_id_idx ON public.couple USING btree (woman_id);
