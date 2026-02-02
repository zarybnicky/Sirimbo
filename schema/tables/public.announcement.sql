CREATE TABLE public.announcement (
    id bigint NOT NULL,
    tenant_id bigint DEFAULT public.current_tenant_id() NOT NULL,
    author_id bigint,
    title text NOT NULL,
    body text NOT NULL,
    is_locked boolean DEFAULT false NOT NULL,
    is_visible boolean DEFAULT true NOT NULL,
    is_sticky boolean DEFAULT false NOT NULL,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    updated_at timestamp with time zone,
    scheduled_since timestamp with time zone,
    scheduled_until timestamp with time zone
);

COMMENT ON TABLE public.announcement IS '@omit create';

GRANT ALL ON TABLE public.announcement TO anonymous;
ALTER TABLE public.announcement ENABLE ROW LEVEL SECURITY;

ALTER TABLE ONLY public.announcement
    ADD CONSTRAINT announcement_pkey PRIMARY KEY (id);
ALTER TABLE ONLY public.announcement
    ADD CONSTRAINT announcement_author_id_fkey FOREIGN KEY (author_id) REFERENCES public.users(id) ON UPDATE RESTRICT ON DELETE RESTRICT;
ALTER TABLE ONLY public.announcement
    ADD CONSTRAINT announcement_tenant_id_fkey FOREIGN KEY (tenant_id) REFERENCES public.tenant(id) ON DELETE CASCADE;

CREATE POLICY admin_all ON public.announcement TO administrator USING (true) WITH CHECK (true);
CREATE POLICY current_tenant ON public.announcement AS RESTRICTIVE USING ((tenant_id = public.current_tenant_id()));
CREATE POLICY member_view ON public.announcement FOR SELECT TO member USING (true);

CREATE TRIGGER _100_timestamps BEFORE INSERT OR UPDATE ON public.announcement FOR EACH ROW EXECUTE FUNCTION app_private.tg__timestamps();
CREATE TRIGGER _200_author BEFORE INSERT OR UPDATE ON public.announcement FOR EACH ROW EXECUTE FUNCTION app_private.tg_announcement__author();
CREATE TRIGGER _600_notify_announcement_insert AFTER INSERT ON public.announcement REFERENCING NEW TABLE AS newtable FOR EACH STATEMENT EXECUTE FUNCTION app_private.tg_announcement__after_write();
CREATE TRIGGER _600_notify_announcement_update AFTER UPDATE ON public.announcement REFERENCING OLD TABLE AS oldtable NEW TABLE AS newtable FOR EACH STATEMENT EXECUTE FUNCTION app_private.tg_announcement__after_write();

CREATE INDEX announcement_author_id_idx ON public.announcement USING btree (author_id);
CREATE INDEX announcement_created_at_idx ON public.announcement USING btree (created_at);
CREATE INDEX announcement_tenant_id_idx ON public.announcement USING btree (tenant_id);
