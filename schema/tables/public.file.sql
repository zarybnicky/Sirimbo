CREATE TABLE public.file (
    id bigint NOT NULL,
    tenant_id bigint DEFAULT public.current_tenant_id() NOT NULL,
    object_key text NOT NULL,
    name text NOT NULL,
    content_type text,
    byte_size bigint,
    uploaded_by bigint DEFAULT public.current_user_id(),
    uploaded_at timestamp with time zone,
    created_at timestamp with time zone DEFAULT now() NOT NULL
);

COMMENT ON TABLE public.file IS '@omit create,update,delete';

GRANT ALL ON TABLE public.file TO anonymous;
ALTER TABLE public.file ENABLE ROW LEVEL SECURITY;

ALTER TABLE ONLY public.file
    ADD CONSTRAINT file_object_key_key UNIQUE (object_key);
ALTER TABLE ONLY public.file
    ADD CONSTRAINT file_pkey PRIMARY KEY (id);
ALTER TABLE ONLY public.file
    ADD CONSTRAINT file_tenant_id_fkey FOREIGN KEY (tenant_id) REFERENCES public.tenant(id) ON DELETE CASCADE;
ALTER TABLE ONLY public.file
    ADD CONSTRAINT file_uploaded_by_fkey FOREIGN KEY (uploaded_by) REFERENCES public.users(id) ON DELETE SET NULL;

CREATE POLICY admin_all ON public.file TO administrator USING (true);
CREATE POLICY current_tenant ON public.file AS RESTRICTIVE USING ((tenant_id = ( SELECT public.current_tenant_id() AS current_tenant_id)));
CREATE POLICY uploader_manage ON public.file TO trainer USING ((uploaded_by = ( SELECT public.current_user_id() AS current_user_id)));
CREATE POLICY visible ON public.file FOR SELECT USING ((id IN ( SELECT visible.id
   FROM app_private.visible_file_ids() visible(id))));

CREATE TRIGGER _900_delete_object AFTER DELETE ON public.file FOR EACH ROW EXECUTE FUNCTION app_private.tg_file__delete();

CREATE UNIQUE INDEX file_tenant_id_id_idx ON public.file USING btree (tenant_id, id);
