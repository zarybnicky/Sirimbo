CREATE TABLE public.event_saved_search (
    id bigint NOT NULL,
    tenant_id bigint DEFAULT public.current_tenant_id() NOT NULL,
    user_id bigint DEFAULT public.current_user_id() NOT NULL,
    name text NOT NULL,
    condition public.event_search_condition NOT NULL,
    feed_key uuid DEFAULT gen_random_uuid(),
    cross_tenant boolean DEFAULT false NOT NULL
);

COMMENT ON TABLE public.event_saved_search IS '@behavior +list -connection';
COMMENT ON COLUMN public.event_saved_search.tenant_id IS '@behavior -attribute:insert -attribute:update';
COMMENT ON COLUMN public.event_saved_search.user_id IS '@behavior -attribute:insert -attribute:update';
COMMENT ON COLUMN public.event_saved_search.condition IS '@behavior -attribute:filterBy -attribute:orderBy';
COMMENT ON COLUMN public.event_saved_search.feed_key IS '@behavior -attribute:filterBy -attribute:orderBy';

GRANT SELECT,DELETE ON TABLE public.event_saved_search TO anonymous;
GRANT INSERT(name),UPDATE(name) ON TABLE public.event_saved_search TO anonymous;
GRANT INSERT(condition),UPDATE(condition) ON TABLE public.event_saved_search TO anonymous;
GRANT INSERT(feed_key),UPDATE(feed_key) ON TABLE public.event_saved_search TO anonymous;
GRANT INSERT(cross_tenant),UPDATE(cross_tenant) ON TABLE public.event_saved_search TO anonymous;
ALTER TABLE public.event_saved_search ENABLE ROW LEVEL SECURITY;

ALTER TABLE ONLY public.event_saved_search
    ADD CONSTRAINT event_saved_search_feed_key_key UNIQUE (feed_key);
ALTER TABLE ONLY public.event_saved_search
    ADD CONSTRAINT event_saved_search_pkey PRIMARY KEY (id);
ALTER TABLE ONLY public.event_saved_search
    ADD CONSTRAINT event_saved_search_tenant_id_fkey FOREIGN KEY (tenant_id) REFERENCES public.tenant(id) ON DELETE CASCADE;
ALTER TABLE ONLY public.event_saved_search
    ADD CONSTRAINT event_saved_search_user_id_fkey FOREIGN KEY (user_id) REFERENCES public.users(id) ON DELETE CASCADE;
COMMENT ON CONSTRAINT event_saved_search_feed_key_key ON public.event_saved_search IS '@behavior -query:resource:single -constraint:resource:update -constraint:resource:delete';

CREATE POLICY owner ON public.event_saved_search USING ((user_id = ( SELECT public.current_user_id() AS current_user_id))) WITH CHECK ((user_id = ( SELECT public.current_user_id() AS current_user_id)));

CREATE INDEX event_saved_search_user_id_idx ON public.event_saved_search USING btree (user_id, tenant_id);
