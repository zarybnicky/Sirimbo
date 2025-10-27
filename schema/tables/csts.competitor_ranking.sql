CREATE TABLE csts.competitor_ranking (
    competitor_id integer NOT NULL,
    discipline text NOT NULL,
    ranking_points_age text NOT NULL,
    ranking_age text NOT NULL,
    competitor_age text NOT NULL,
    series text NOT NULL,
    competitors text NOT NULL,
    class text,
    points integer,
    domestic_finale_count integer,
    foreign_finale_count integer,
    ranklist_ranking integer,
    ranklist_points integer,
    athlete_idt integer,
    couple_id integer,
    CONSTRAINT competitor_ranking_check CHECK ((((athlete_idt IS NOT NULL) AND (couple_id IS NULL)) OR ((athlete_idt IS NULL) AND (couple_id IS NOT NULL))))
);

ALTER TABLE ONLY csts.competitor_ranking
    ADD CONSTRAINT competitor_ranking_athlete_idt_discipline_key UNIQUE (athlete_idt, discipline);
ALTER TABLE ONLY csts.competitor_ranking
    ADD CONSTRAINT competitor_ranking_couple_id_discipline_key UNIQUE (couple_id, discipline);
ALTER TABLE ONLY csts.competitor_ranking
    ADD CONSTRAINT competitor_ranking_pkey PRIMARY KEY (competitor_id, discipline);
ALTER TABLE ONLY csts.competitor_ranking
    ADD CONSTRAINT competitor_ranking_athlete_idt_fkey FOREIGN KEY (athlete_idt) REFERENCES csts.athlete(idt) ON DELETE CASCADE;
ALTER TABLE ONLY csts.competitor_ranking
    ADD CONSTRAINT competitor_ranking_couple_id_fkey FOREIGN KEY (couple_id) REFERENCES csts.couple(id) ON DELETE CASCADE;

CREATE INDEX competitor_ranking_athlete_idx ON csts.competitor_ranking USING btree (athlete_idt);
CREATE INDEX competitor_ranking_couple_idx ON csts.competitor_ranking USING btree (couple_id);
