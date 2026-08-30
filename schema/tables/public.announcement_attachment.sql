CREATE TABLE public.announcement_attachment (
    tenant_id bigint DEFAULT public.current_tenant_id() NOT NULL,
    announcement_id bigint NOT NULL,
    file_id bigint NOT NULL,
    inline boolean DEFAULT false NOT NULL
);

COMMENT ON TABLE public.announcement_attachment IS '@omit create,update,delete';

GRANT ALL ON TABLE public.announcement_attachment TO anonymous;
ALTER TABLE public.announcement_attachment ENABLE ROW LEVEL SECURITY;

ALTER TABLE ONLY public.announcement_attachment
    ADD CONSTRAINT announcement_attachment_pkey PRIMARY KEY (tenant_id, announcement_id, file_id);
ALTER TABLE ONLY public.announcement_attachment
    ADD CONSTRAINT announcement_attachment_announcement_fk FOREIGN KEY (tenant_id, announcement_id) REFERENCES public.announcement(tenant_id, id) ON DELETE CASCADE;
ALTER TABLE ONLY public.announcement_attachment
    ADD CONSTRAINT announcement_attachment_file_fk FOREIGN KEY (tenant_id, file_id) REFERENCES public.file(tenant_id, id) ON DELETE CASCADE;

CREATE POLICY admin_all ON public.announcement_attachment TO administrator USING (true);
CREATE POLICY current_tenant ON public.announcement_attachment AS RESTRICTIVE USING ((tenant_id = ( SELECT public.current_tenant_id() AS current_tenant_id)));
CREATE POLICY member_view ON public.announcement_attachment FOR SELECT USING ((announcement_id IN ( SELECT announcement.id
   FROM public.announcement)));
CREATE POLICY trainer_manage ON public.announcement_attachment TO trainer USING ((announcement_id IN ( SELECT announcement.id
   FROM public.announcement
  WHERE (announcement.author_id = ( SELECT public.current_user_id() AS current_user_id)))));

CREATE INDEX announcement_attachment_file_idx ON public.announcement_attachment USING btree (tenant_id, file_id);
