CREATE TABLE public.room (
    id bigint NOT NULL,
    name text NOT NULL,
    description jsonb NOT NULL,
    location_id bigint
);

GRANT ALL ON TABLE public.room TO anonymous;
ALTER TABLE public.room ENABLE ROW LEVEL SECURITY;

ALTER TABLE ONLY public.room
    ADD CONSTRAINT room_pkey PRIMARY KEY (id);
ALTER TABLE ONLY public.room
    ADD CONSTRAINT room_location_fkey FOREIGN KEY (location_id) REFERENCES public.location(id) ON DELETE CASCADE;

CREATE POLICY admin_all ON public.room TO administrator USING (true) WITH CHECK (true);
CREATE POLICY public_view ON public.room FOR SELECT TO anonymous USING (true);
