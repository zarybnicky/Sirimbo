CREATE TABLE public.session (
    ss_id character varying(128) NOT NULL,
    ss_updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    ss_lifetime bigint NOT NULL,
    ss_user bigint
);

COMMENT ON TABLE public.session IS '@omit create,update,delete';

GRANT ALL ON TABLE public.session TO anonymous;
ALTER TABLE public.session ENABLE ROW LEVEL SECURITY;

ALTER TABLE ONLY public.session
    ADD CONSTRAINT idx_23925_primary PRIMARY KEY (ss_id);
ALTER TABLE ONLY public.session
    ADD CONSTRAINT session_ss_user_fkey FOREIGN KEY (ss_user) REFERENCES public.users(u_id) ON DELETE CASCADE;

CREATE POLICY admin_all ON public.session TO administrator USING (true) WITH CHECK (true);
CREATE POLICY manage_own ON public.session USING ((ss_user = public.current_user_id())) WITH CHECK ((ss_user = public.current_user_id()));

CREATE INDEX ss_user ON public.session USING btree (ss_user);