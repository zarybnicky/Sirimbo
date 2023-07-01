CREATE TABLE public.permissions (
    pe_id bigint NOT NULL,
    pe_name text NOT NULL,
    pe_description text NOT NULL,
    pe_akce integer NOT NULL,
    pe_aktuality integer NOT NULL,
    pe_ankety integer NOT NULL,
    pe_dokumenty integer NOT NULL,
    pe_galerie integer NOT NULL,
    pe_inzerce integer NOT NULL,
    pe_konzole integer NOT NULL,
    pe_nabidka integer NOT NULL,
    pe_nastenka integer NOT NULL,
    pe_novinky integer NOT NULL,
    pe_pary integer NOT NULL,
    pe_platby integer NOT NULL,
    pe_permissions integer NOT NULL,
    pe_rozpis integer NOT NULL,
    pe_skupiny integer NOT NULL,
    pe_users integer NOT NULL,
    pe_main integer NOT NULL,
    id bigint GENERATED ALWAYS AS (pe_id) STORED
);

GRANT ALL ON TABLE public.permissions TO anonymous;
ALTER TABLE public.permissions ENABLE ROW LEVEL SECURITY;

ALTER TABLE ONLY public.permissions
    ADD CONSTRAINT idx_23846_primary PRIMARY KEY (pe_id);

CREATE POLICY admin_all ON public.permissions TO administrator USING (true) WITH CHECK (true);
CREATE POLICY all_view ON public.permissions FOR SELECT USING (true);

