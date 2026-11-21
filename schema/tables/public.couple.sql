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
    name text NOT NULL,
    short_name text NOT NULL,
    search_vector tsvector GENERATED ALWAYS AS (to_tsvector('simple'::regconfig, app_private.normalize_name(name))) STORED,
    CONSTRAINT couple_until_gt_since CHECK ((until > since))
);

COMMENT ON TABLE public.couple IS '@behavior +list -connection +query:resource:list -query:resource:connection
@ref federationCompetitors via:public.couple_federation_link;federated.competitor
@ref federationRanklistEntries via:public.couple_federation_link;federated.competitor;federated.ranklist_entry';
COMMENT ON COLUMN public.couple.legacy_pary_id IS '@behavior -*';
COMMENT ON COLUMN public.couple.active_range IS '@behavior -*';
COMMENT ON COLUMN public.couple.name IS '@behavior -attribute:insert -attribute:update';
COMMENT ON COLUMN public.couple.short_name IS '@behavior -attribute:insert -attribute:update';
COMMENT ON COLUMN public.couple.search_vector IS '@behavior -*';

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
COMMENT ON CONSTRAINT couple_image_fk ON public.couple IS '@fieldName image';

CREATE POLICY admin_delete ON public.couple FOR DELETE TO administrator USING ((app_private.can_edit_person(man_id) OR app_private.can_edit_person(woman_id)));
CREATE POLICY admin_insert ON public.couple FOR INSERT TO administrator WITH CHECK ((app_private.can_edit_person(man_id) OR app_private.can_edit_person(woman_id)));
CREATE POLICY admin_update ON public.couple FOR UPDATE TO administrator USING ((app_private.can_edit_person(man_id) OR app_private.can_edit_person(woman_id)));
CREATE POLICY event_share_view ON public.couple FOR SELECT TO anonymous USING ((id IN ( SELECT event_registration.couple_id
   FROM public.event_registration
  WHERE ((event_registration.registration_status = 'active'::public.event_registration_status) AND (event_registration.parent_registration_id IS NULL) AND (event_registration.couple_id IS NOT NULL) AND (event_registration.event_id = ANY (( SELECT app_private.current_shared_event_ids() AS current_shared_event_ids)::bigint[]))))));
CREATE POLICY system_admin_all ON public.couple TO system_admin USING (true);
CREATE POLICY view_visible_person ON public.couple FOR SELECT USING ((ARRAY[man_id, woman_id] && ( SELECT array_agg(DISTINCT id.id) AS array_agg
   FROM app_private.visible_person_ids(false) id(id))));

CREATE TRIGGER _050_names BEFORE INSERT OR UPDATE OF man_id, woman_id, name, short_name ON public.couple FOR EACH ROW EXECUTE FUNCTION app_private.tg_couple__names();
CREATE TRIGGER _050_relationship_status BEFORE INSERT OR UPDATE OF since, until ON public.couple FOR EACH ROW EXECUTE FUNCTION app_private.tg_relationship__status();
CREATE TRIGGER _100_timestamps BEFORE INSERT OR UPDATE ON public.couple FOR EACH ROW EXECUTE FUNCTION app_private.tg__timestamps();

CREATE INDEX couple_image_idx ON public.couple USING btree (image_id) WHERE (image_id IS NOT NULL);
CREATE INDEX couple_man_active_idx ON public.couple USING btree (man_id) INCLUDE (id) WHERE (status = 'active'::public.relationship_status);
CREATE INDEX couple_man_active_lookup ON public.couple USING btree (man_id, since, until, id) INCLUDE (woman_id, status) WHERE (status = 'active'::public.relationship_status);
CREATE INDEX couple_man_id_idx ON public.couple USING btree (man_id);
CREATE INDEX couple_search_vector_idx ON public.couple USING gin (search_vector);
CREATE INDEX couple_woman_active_idx ON public.couple USING btree (woman_id) INCLUDE (id) WHERE (status = 'active'::public.relationship_status);
CREATE INDEX couple_woman_active_lookup ON public.couple USING btree (woman_id, since, until, id) INCLUDE (man_id, status) WHERE (status = 'active'::public.relationship_status);
CREATE INDEX couple_woman_id_idx ON public.couple USING btree (woman_id);
