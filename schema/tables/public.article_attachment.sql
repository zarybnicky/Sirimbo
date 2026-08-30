CREATE TABLE public.article_attachment (
    tenant_id bigint DEFAULT public.current_tenant_id() NOT NULL,
    aktuality_id bigint NOT NULL,
    file_id bigint NOT NULL,
    inline boolean DEFAULT false NOT NULL
);

COMMENT ON TABLE public.article_attachment IS '@omit create,update,delete';

GRANT ALL ON TABLE public.article_attachment TO anonymous;
ALTER TABLE public.article_attachment ENABLE ROW LEVEL SECURITY;

ALTER TABLE ONLY public.article_attachment
    ADD CONSTRAINT article_attachment_pkey PRIMARY KEY (tenant_id, aktuality_id, file_id);
ALTER TABLE ONLY public.article_attachment
    ADD CONSTRAINT article_attachment_article_fk FOREIGN KEY (tenant_id, aktuality_id) REFERENCES public.aktuality(tenant_id, id) ON DELETE CASCADE;
ALTER TABLE ONLY public.article_attachment
    ADD CONSTRAINT article_attachment_file_fk FOREIGN KEY (tenant_id, file_id) REFERENCES public.file(tenant_id, id) ON DELETE CASCADE;

CREATE POLICY admin_all ON public.article_attachment TO administrator USING (true);
CREATE POLICY current_tenant ON public.article_attachment AS RESTRICTIVE USING ((tenant_id = ( SELECT public.current_tenant_id() AS current_tenant_id)));
CREATE POLICY public_view ON public.article_attachment FOR SELECT USING ((aktuality_id IN ( SELECT aktuality.id
   FROM public.aktuality)));

CREATE INDEX article_attachment_file_idx ON public.article_attachment USING btree (tenant_id, file_id);
