CREATE TABLE csts.athlete (
    idt integer NOT NULL,
    name text NOT NULL,
    age_category text NOT NULL,
    sex text NOT NULL,
    medical_checkup_expiration date,
    fetched_at timestamp with time zone DEFAULT now() NOT NULL
);

ALTER TABLE ONLY csts.athlete
    ADD CONSTRAINT athlete_pkey PRIMARY KEY (idt);
