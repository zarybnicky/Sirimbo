CREATE TABLE federated.competition (
    id bigint NOT NULL,
    federation text NOT NULL,
    external_id text NOT NULL,
    event_id bigint NOT NULL,
    category_id bigint NOT NULL,
    start_date date NOT NULL,
    end_date date,
    check_in_end time without time zone,
    registration_fee numeric(10,3),
    participants_total integer,
    excused_total integer,
    completed_at timestamp with time zone,
    competition_type federated.competition_type,
    status federated.event_status DEFAULT 'unknown'::federated.event_status NOT NULL,
    CONSTRAINT competition_check CHECK (((end_date IS NULL) OR (end_date >= start_date)))
);

COMMENT ON TABLE federated.competition IS '@name competition
@behavior -insert -update -delete -list';

GRANT SELECT ON TABLE federated.competition TO anonymous;

ALTER TABLE ONLY federated.competition
    ADD CONSTRAINT competition_federation_external_id_key UNIQUE (federation, external_id);
ALTER TABLE ONLY federated.competition
    ADD CONSTRAINT competition_federation_id_key UNIQUE (federation, id);
ALTER TABLE ONLY federated.competition
    ADD CONSTRAINT competition_id_category_id_key UNIQUE (id, category_id);
ALTER TABLE ONLY federated.competition
    ADD CONSTRAINT competition_id_event_id_key UNIQUE (id, event_id);
ALTER TABLE ONLY federated.competition
    ADD CONSTRAINT competition_pkey PRIMARY KEY (id);
ALTER TABLE ONLY federated.competition
    ADD CONSTRAINT competition_category_id_fkey FOREIGN KEY (category_id) REFERENCES federated.category(id);
ALTER TABLE ONLY federated.competition
    ADD CONSTRAINT competition_event_id_fkey FOREIGN KEY (event_id) REFERENCES federated.event(id);
ALTER TABLE ONLY federated.competition
    ADD CONSTRAINT competition_federation_event_id_fkey FOREIGN KEY (federation, event_id) REFERENCES federated.event(federation, id);
ALTER TABLE ONLY federated.competition
    ADD CONSTRAINT competition_federation_fkey FOREIGN KEY (federation) REFERENCES federated.federation(code);

CREATE TRIGGER _900_refresh_competitor_activity_dates AFTER UPDATE ON federated.competition REFERENCING OLD TABLE AS oldtable NEW TABLE AS newtable FOR EACH STATEMENT EXECUTE FUNCTION app_private.tg_competition_dates__refresh_competitor_activity();

CREATE INDEX competition_category_id_idx ON federated.competition USING btree (category_id);
CREATE INDEX competition_event_id_idx ON federated.competition USING btree (event_id);
CREATE INDEX competition_start_date_idx ON federated.competition USING btree (start_date) INCLUDE (id);
