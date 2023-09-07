CREATE TABLE public.attachment (
    object_name text NOT NULL,
    preview_object_name text,
    uploaded_by bigint DEFAULT public.current_user_id(),
    uploaded_at timestamp with time zone DEFAULT now() NOT NULL,
    thumbhash text,
    width integer,
    height integer
);

COMMENT ON TABLE public.attachment IS '@omit update';

GRANT ALL ON TABLE public.attachment TO anonymous;
ALTER TABLE public.attachment ENABLE ROW LEVEL SECURITY;

ALTER TABLE ONLY public.attachment
    ADD CONSTRAINT attachment_pkey PRIMARY KEY (object_name);
ALTER TABLE ONLY public.attachment
    ADD CONSTRAINT attachment_uploaded_by_fkey FOREIGN KEY (uploaded_by) REFERENCES public.users(u_id) ON DELETE SET NULL;

CREATE POLICY admin_all ON public.attachment TO administrator USING (true) WITH CHECK (true);
CREATE POLICY public_view ON public.attachment FOR SELECT TO anonymous USING (true);

CREATE INDEX uploaded_by ON public.attachment USING btree (uploaded_by);