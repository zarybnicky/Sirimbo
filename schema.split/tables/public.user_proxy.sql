CREATE TABLE public.user_proxy (
    user_id bigint NOT NULL,
    person_id bigint NOT NULL,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    updated_at timestamp with time zone DEFAULT now() NOT NULL,
    id bigint NOT NULL,
    since timestamp with time zone,
    until timestamp with time zone,
    active_range tstzrange GENERATED ALWAYS AS (tstzrange(since, until, '[]'::text)) STORED NOT NULL,
    status public.relationship_status DEFAULT 'active'::public.relationship_status NOT NULL,
    active boolean GENERATED ALWAYS AS ((status = 'active'::public.relationship_status)) STORED NOT NULL
);

COMMENT ON TABLE public.user_proxy IS '@simpleCollections only';
COMMENT ON COLUMN public.user_proxy.active_range IS '@omit';

GRANT ALL ON TABLE public.user_proxy TO anonymous;
ALTER TABLE public.user_proxy ENABLE ROW LEVEL SECURITY;

ALTER TABLE ONLY public.user_proxy
    ADD CONSTRAINT user_proxy_pkey PRIMARY KEY (id);
ALTER TABLE ONLY public.user_proxy
    ADD CONSTRAINT user_proxy_person_id_fkey FOREIGN KEY (person_id) REFERENCES public.person(id) ON UPDATE CASCADE ON DELETE CASCADE;
ALTER TABLE ONLY public.user_proxy
    ADD CONSTRAINT user_proxy_user_id_fkey FOREIGN KEY (user_id) REFERENCES public.users(u_id) ON UPDATE CASCADE ON DELETE CASCADE;

CREATE POLICY admin_all ON public.user_proxy TO administrator USING (true);
CREATE POLICY view_personal ON public.user_proxy FOR SELECT USING ((user_id = public.current_user_id()));

CREATE TRIGGER _100_timestamps BEFORE INSERT OR UPDATE ON public.user_proxy FOR EACH ROW EXECUTE FUNCTION app_private.tg__timestamps();

CREATE INDEX user_proxy_person_id_idx ON public.user_proxy USING btree (person_id);
CREATE INDEX user_proxy_range_idx ON public.user_proxy USING gist (active_range, person_id, user_id);
CREATE INDEX user_proxy_user_id_idx ON public.user_proxy USING btree (user_id);