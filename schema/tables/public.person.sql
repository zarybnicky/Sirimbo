CREATE TABLE public.person (
    id bigint NOT NULL,
    first_name text NOT NULL,
    last_name text NOT NULL,
    gender public.gender_type NOT NULL,
    birth_date date,
    nationality text NOT NULL,
    tax_identification_number text,
    national_id_number text,
    csts_id integer,
    wdsf_id integer,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    updated_at timestamp with time zone DEFAULT now() NOT NULL,
    legacy_user_id bigint,
    prefix_title text DEFAULT ''::text NOT NULL,
    suffix_title text DEFAULT ''::text NOT NULL,
    bio text DEFAULT ''::text NOT NULL,
    email public.citext,
    phone text,
    name text GENERATED ALWAYS AS (public.immutable_concat_ws(' '::text, VARIADIC ARRAY[NULLIF(btrim(prefix_title), ''::text), NULLIF(btrim(first_name), ''::text), NULLIF(btrim(last_name), ''::text),
CASE
    WHEN (btrim(suffix_title) = ''::text) THEN NULL::text
    ELSE (', '::text || btrim(suffix_title))
END])) STORED NOT NULL,
    address public.address_domain,
    external_ids text[],
    note text DEFAULT ''::text NOT NULL,
    search_name text GENERATED ALWAYS AS (app_private.normalize_name(public.immutable_concat_ws(' '::text, VARIADIC ARRAY[first_name, last_name]))) STORED,
    instagram_username text,
    tiktok_username text,
    facebook_url text,
    website_url text,
    image_id bigint,
    search_vector tsvector GENERATED ALWAYS AS (to_tsvector('simple'::regconfig, app_private.normalize_name(public.immutable_concat_ws(' '::text, VARIADIC ARRAY[first_name, last_name])))) STORED
);

COMMENT ON TABLE public.person IS '@behavior -insert
@ref federationPeople via:public.person_federation_link;federated.person
@ref federationCategoryProgresses via:public.person_federation_link;federated.person;federated.competitor_component;federated.competitor;federated.competitor_category_progress
@ref federationRanklistEntries via:public.person_federation_link;federated.person;federated.competitor_component;federated.competitor;federated.ranklist_entry';
COMMENT ON COLUMN public.person.legacy_user_id IS '@behavior -*';
COMMENT ON COLUMN public.person.search_name IS '@behavior -*';
COMMENT ON COLUMN public.person.search_vector IS '@behavior -*';

GRANT ALL ON TABLE public.person TO anonymous;
ALTER TABLE public.person ENABLE ROW LEVEL SECURITY;

ALTER TABLE ONLY public.person
    ADD CONSTRAINT person_pkey PRIMARY KEY (id);
ALTER TABLE ONLY public.person
    ADD CONSTRAINT person_image_fk FOREIGN KEY (image_id) REFERENCES public.file(id) ON DELETE SET NULL;
COMMENT ON CONSTRAINT person_image_fk ON public.person IS '@fieldName image';

CREATE POLICY admin_delete ON public.person FOR DELETE TO administrator USING (app_private.can_edit_person(id));
CREATE POLICY admin_myself ON public.person FOR UPDATE USING ((id = ANY (( SELECT public.current_person_ids() AS current_person_ids)::bigint[])));
CREATE POLICY admin_update ON public.person FOR UPDATE TO administrator USING (app_private.can_edit_person(id));
CREATE POLICY event_share_view ON public.person FOR SELECT TO anonymous USING ((id IN ( SELECT event_registration.person_id
   FROM public.event_registration
  WHERE ((event_registration.registration_status = 'active'::public.event_registration_status) AND (event_registration.person_id IS NOT NULL) AND (event_registration.event_id = ANY (( SELECT app_private.current_shared_event_ids() AS current_shared_event_ids)::bigint[])))
UNION ALL
 SELECT event_trainer.person_id
   FROM public.event_trainer
  WHERE (event_trainer.event_id = ANY (( SELECT app_private.current_shared_event_ids() AS current_shared_event_ids)::bigint[])))));
CREATE POLICY system_admin_all ON public.person TO system_admin USING (true);
CREATE POLICY view_tenant_or_trainer ON public.person FOR SELECT USING ((id IN ( SELECT app_private.visible_person_ids(true) AS visible_person_ids)));

CREATE TRIGGER _100_timestamps BEFORE INSERT OR UPDATE ON public.person FOR EACH ROW EXECUTE FUNCTION app_private.tg__timestamps();
CREATE TRIGGER _500_couple_names AFTER UPDATE OF first_name, last_name ON public.person FOR EACH ROW WHEN (((old.first_name IS DISTINCT FROM new.first_name) OR (old.last_name IS DISTINCT FROM new.last_name))) EXECUTE FUNCTION app_private.tg_person__couple_names();

CREATE INDEX person_csts_id_idx ON public.person USING btree (csts_id) WHERE (csts_id IS NOT NULL);
CREATE INDEX person_image_idx ON public.person USING btree (image_id) WHERE (image_id IS NOT NULL);
CREATE INDEX person_search_vector_idx ON public.person USING gin (search_vector);
