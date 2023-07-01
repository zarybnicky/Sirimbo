CREATE TABLE public.page (
    id integer NOT NULL,
    url character varying NOT NULL,
    content json NOT NULL,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    updated_at timestamp with time zone DEFAULT now() NOT NULL,
    title text DEFAULT ''::text NOT NULL
);

COMMENT ON TABLE public.page IS '@omit delete';

GRANT ALL ON TABLE public.page TO anonymous;
ALTER TABLE public.page ENABLE ROW LEVEL SECURITY;

ALTER TABLE ONLY public.page
    ADD CONSTRAINT page_pkey PRIMARY KEY (id);
ALTER TABLE ONLY public.page
    ADD CONSTRAINT page_url_key UNIQUE (url);

CREATE POLICY admin_all ON public.page TO administrator USING (true) WITH CHECK (true);
CREATE POLICY all_view ON public.page FOR SELECT USING (true);

CREATE TRIGGER _100_page_revision AFTER INSERT OR DELETE OR UPDATE ON public.page FOR EACH ROW EXECUTE FUNCTION app_private.insert_revision();
CREATE TRIGGER _100_timestamps BEFORE INSERT OR UPDATE ON public.page FOR EACH ROW EXECUTE FUNCTION app_private.tg__timestamps();
