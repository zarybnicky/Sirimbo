CREATE TABLE csts.couple (
    id integer NOT NULL,
    couple_idt integer NOT NULL,
    man_idt integer NOT NULL,
    woman_idt integer NOT NULL,
    formed_at timestamp with time zone NOT NULL
);

ALTER TABLE ONLY csts.couple
    ADD CONSTRAINT couple_couple_idt_key UNIQUE (couple_idt);
ALTER TABLE ONLY csts.couple
    ADD CONSTRAINT couple_pkey PRIMARY KEY (id);
ALTER TABLE ONLY csts.couple
    ADD CONSTRAINT couple_man_idt_fkey FOREIGN KEY (man_idt) REFERENCES csts.athlete(idt) ON DELETE CASCADE;
ALTER TABLE ONLY csts.couple
    ADD CONSTRAINT couple_woman_idt_fkey FOREIGN KEY (woman_idt) REFERENCES csts.athlete(idt) ON DELETE CASCADE;

CREATE INDEX couple_man_idx ON csts.couple USING btree (man_idt);
CREATE INDEX couple_woman_idx ON csts.couple USING btree (woman_idt);
