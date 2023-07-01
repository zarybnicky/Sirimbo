CREATE TABLE public.room (
    id bigint NOT NULL,
    name text NOT NULL,
    description jsonb NOT NULL,
    location bigint
);

GRANT ALL ON TABLE public.room TO anonymous;
ALTER TABLE public.room ENABLE ROW LEVEL SECURITY;

ALTER TABLE ONLY public.room
    ADD CONSTRAINT room_pkey PRIMARY KEY (id);
ALTER TABLE ONLY public.room
    ADD CONSTRAINT room_location_fkey FOREIGN KEY (location) REFERENCES public.location(id);

CREATE POLICY admin_all ON public.room TO administrator USING (true) WITH CHECK (true);
CREATE POLICY public_view ON public.room FOR SELECT TO anonymous USING (true);

CREATE INDEX room_location_idx ON public.room USING btree (location);