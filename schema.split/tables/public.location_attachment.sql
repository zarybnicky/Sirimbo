CREATE TABLE public.location_attachment (
    location_id bigint NOT NULL,
    object_name text NOT NULL
);

COMMENT ON TABLE public.location_attachment IS '@omit create,update,delete';

GRANT ALL ON TABLE public.location_attachment TO anonymous;
ALTER TABLE public.location_attachment ENABLE ROW LEVEL SECURITY;

ALTER TABLE ONLY public.location_attachment
    ADD CONSTRAINT location_attachment_pkey PRIMARY KEY (location_id, object_name);
ALTER TABLE ONLY public.location_attachment
    ADD CONSTRAINT location_attachment_location_id_fkey FOREIGN KEY (location_id) REFERENCES public.location(id) ON DELETE CASCADE;
ALTER TABLE ONLY public.location_attachment
    ADD CONSTRAINT location_attachment_object_name_fkey FOREIGN KEY (object_name) REFERENCES public.attachment(object_name) ON DELETE CASCADE;

CREATE POLICY admin_all ON public.location_attachment TO administrator USING (true) WITH CHECK (true);
CREATE POLICY public_view ON public.location_attachment FOR SELECT TO anonymous USING (true);

CREATE INDEX object_name ON public.location_attachment USING btree (object_name);