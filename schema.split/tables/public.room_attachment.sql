CREATE TABLE public.room_attachment (
    room_id bigint NOT NULL,
    object_name text NOT NULL
);

COMMENT ON TABLE public.room_attachment IS '@omit create,update,delete';

GRANT ALL ON TABLE public.room_attachment TO anonymous;
ALTER TABLE public.room_attachment ENABLE ROW LEVEL SECURITY;

ALTER TABLE ONLY public.room_attachment
    ADD CONSTRAINT room_attachment_pkey PRIMARY KEY (room_id, object_name);
ALTER TABLE ONLY public.room_attachment
    ADD CONSTRAINT room_attachment_object_name_fkey FOREIGN KEY (object_name) REFERENCES public.attachment(object_name);
ALTER TABLE ONLY public.room_attachment
    ADD CONSTRAINT room_attachment_room_id_fkey FOREIGN KEY (room_id) REFERENCES public.room(id);

CREATE POLICY admin_all ON public.room_attachment TO administrator USING (true) WITH CHECK (true);
CREATE POLICY public_view ON public.room_attachment FOR SELECT TO anonymous USING (true);

CREATE INDEX room_attachment_object_name_idx ON public.room_attachment USING btree (object_name);