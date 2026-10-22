CREATE TABLE app_private.system_admin_user (
    user_id bigint NOT NULL,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    created_by bigint DEFAULT public.current_user_id() NOT NULL
);

COMMENT ON TABLE app_private.system_admin_user IS '@omit all';

ALTER TABLE ONLY app_private.system_admin_user
    ADD CONSTRAINT system_admin_user_pkey PRIMARY KEY (user_id);
ALTER TABLE ONLY app_private.system_admin_user
    ADD CONSTRAINT system_admin_user_user_id_fkey FOREIGN KEY (user_id) REFERENCES public.users(id) ON DELETE CASCADE;
