CREATE TABLE federated.competitor_club_affiliation (
    competitor_id bigint NOT NULL,
    club_id bigint NOT NULL,
    valid_from date NOT NULL,
    valid_to date,
    CONSTRAINT competitor_club_affiliation_check CHECK (((valid_to IS NULL) OR (valid_to >= valid_from)))
);

ALTER TABLE ONLY federated.competitor_club_affiliation
    ADD CONSTRAINT competitor_club_affiliation_competitor_id_club_id_daterang_excl EXCLUDE USING gist (competitor_id WITH =, club_id WITH =, daterange(valid_from, COALESCE(valid_to, 'infinity'::date), '[]'::text) WITH &&);
ALTER TABLE ONLY federated.competitor_club_affiliation
    ADD CONSTRAINT competitor_club_affiliation_pkey PRIMARY KEY (competitor_id, club_id, valid_from);
ALTER TABLE ONLY federated.competitor_club_affiliation
    ADD CONSTRAINT competitor_club_affiliation_club_id_fkey FOREIGN KEY (club_id) REFERENCES federated.federation_club(id);
ALTER TABLE ONLY federated.competitor_club_affiliation
    ADD CONSTRAINT competitor_club_affiliation_competitor_id_fkey FOREIGN KEY (competitor_id) REFERENCES federated.competitor(id);

CREATE INDEX competitor_club_affiliation_club_id_valid_from_valid_to_idx ON federated.competitor_club_affiliation USING btree (club_id, valid_from, valid_to);
