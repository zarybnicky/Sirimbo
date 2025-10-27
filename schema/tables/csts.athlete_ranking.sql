CREATE TABLE csts.athlete_ranking (
    athlete_id integer NOT NULL,
    discipline text NOT NULL,
    series text NOT NULL,
    personal_class text,
    personal_points integer,
    personal_domestic_finale_count integer,
    personal_foreign_finale_count integer,
    personal_approved boolean
);

ALTER TABLE ONLY csts.athlete_ranking
    ADD CONSTRAINT athlete_ranking_pkey PRIMARY KEY (athlete_id, discipline, series);
ALTER TABLE ONLY csts.athlete_ranking
    ADD CONSTRAINT athlete_ranking_athlete_id_fkey FOREIGN KEY (athlete_id) REFERENCES csts.athlete(idt) ON DELETE CASCADE;
