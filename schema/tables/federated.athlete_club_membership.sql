CREATE TABLE federated.athlete_club_membership (
    athlete_id bigint NOT NULL,
    club_id bigint NOT NULL,
    valid_from date NOT NULL,
    valid_to date,
    CONSTRAINT athlete_club_membership_check CHECK (((valid_to IS NULL) OR (valid_to >= valid_from)))
);

GRANT SELECT ON TABLE federated.athlete_club_membership TO anonymous;

ALTER TABLE ONLY federated.athlete_club_membership
    ADD CONSTRAINT athlete_club_membership_athlete_id_club_id_daterange_excl EXCLUDE USING gist (athlete_id WITH =, club_id WITH =, daterange(valid_from, COALESCE(valid_to, 'infinity'::date), '[]'::text) WITH &&);
ALTER TABLE ONLY federated.athlete_club_membership
    ADD CONSTRAINT athlete_club_membership_pkey PRIMARY KEY (athlete_id, club_id, valid_from);
ALTER TABLE ONLY federated.athlete_club_membership
    ADD CONSTRAINT athlete_club_membership_athlete_id_fkey FOREIGN KEY (athlete_id) REFERENCES federated.athlete(id);
ALTER TABLE ONLY federated.athlete_club_membership
    ADD CONSTRAINT athlete_club_membership_club_id_fkey FOREIGN KEY (club_id) REFERENCES federated.federation_club(id);

CREATE INDEX athlete_club_membership_athlete_id_valid_from_valid_to_idx ON federated.athlete_club_membership USING btree (athlete_id, valid_from, valid_to);
CREATE INDEX athlete_club_membership_club_id_valid_from_valid_to_idx ON federated.athlete_club_membership USING btree (club_id, valid_from, valid_to);
