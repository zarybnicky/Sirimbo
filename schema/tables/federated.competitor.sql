CREATE TABLE federated.competitor (
    id text GENERATED ALWAYS AS (((federation || ':'::text) || external_id)) STORED NOT NULL,
    federation text NOT NULL,
    competitor_type federated.competitor_type NOT NULL,
    age_group text,
    name text,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    age_group_code text,
    search_name text GENERATED ALWAYS AS (app_private.normalize_name(name)) STORED,
    external_id text CONSTRAINT competitor_external_id_text_not_null NOT NULL
);

COMMENT ON TABLE federated.competitor IS '@name federation_competitor
@behavior -insert -update -delete -list
@ref people via:federated.competitor_component;federated.person;public.person_federation_link;public.person behavior:"+list -connection"
@ref couples via:public.couple_federation_link;public.couple
@ref competitions via:federated.competition_participation;federated.competition';
COMMENT ON COLUMN federated.competitor.age_group IS '@behavior -*';
COMMENT ON COLUMN federated.competitor.age_group_code IS '@name ageGroup
@behavior -insert -update';
COMMENT ON COLUMN federated.competitor.search_name IS '@behavior -*';

GRANT SELECT ON TABLE federated.competitor TO anonymous;

ALTER TABLE ONLY federated.competitor
    ADD CONSTRAINT competitor_federation_external_id_key UNIQUE (federation, external_id);
ALTER TABLE ONLY federated.competitor
    ADD CONSTRAINT competitor_pkey PRIMARY KEY (id);
ALTER TABLE ONLY federated.competitor
    ADD CONSTRAINT competitor_age_group_code_fkey FOREIGN KEY (age_group_code) REFERENCES federated.age_group(code);
ALTER TABLE ONLY federated.competitor
    ADD CONSTRAINT competitor_federation_fkey FOREIGN KEY (federation) REFERENCES federated.federation(code);

CREATE TRIGGER _resolve_age_group BEFORE INSERT OR UPDATE OF age_group ON federated.competitor FOR EACH ROW EXECUTE FUNCTION app_private.tg_resolve_age_group();

CREATE INDEX competitor_age_group_code_idx ON federated.competitor USING btree (age_group_code);
CREATE INDEX competitor_external_id_text_idx ON federated.competitor USING btree (external_id);
CREATE INDEX competitor_search_name_trgm_idx ON federated.competitor USING gin (search_name public.gin_trgm_ops);
