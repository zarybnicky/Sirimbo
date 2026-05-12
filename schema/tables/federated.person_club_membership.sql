CREATE TABLE federated.person_club_membership (
    person_id text NOT NULL,
    club_id bigint NOT NULL,
    valid_from date NOT NULL,
    valid_to date,
    CONSTRAINT person_club_membership_check CHECK (((valid_to IS NULL) OR (valid_to >= valid_from)))
);

GRANT SELECT ON TABLE federated.person_club_membership TO anonymous;

ALTER TABLE ONLY federated.person_club_membership
    ADD CONSTRAINT person_club_membership_person_id_club_id_daterange_excl EXCLUDE USING gist (person_id WITH =, club_id WITH =, daterange(valid_from, COALESCE(valid_to, 'infinity'::date), '[)'::text) WITH &&);
ALTER TABLE ONLY federated.person_club_membership
    ADD CONSTRAINT person_club_membership_pkey PRIMARY KEY (person_id, club_id, valid_from);
ALTER TABLE ONLY federated.person_club_membership
    ADD CONSTRAINT person_club_membership_club_id_fkey FOREIGN KEY (club_id) REFERENCES federated.federation_club(id);
ALTER TABLE ONLY federated.person_club_membership
    ADD CONSTRAINT person_club_membership_person_id_fkey FOREIGN KEY (person_id) REFERENCES federated.person(id);

CREATE INDEX person_club_membership_club_id_valid_from_valid_to_idx ON federated.person_club_membership USING btree (club_id, valid_from, valid_to);
CREATE INDEX person_club_membership_person_id_valid_from_valid_to_idx ON federated.person_club_membership USING btree (person_id, valid_from, valid_to);
