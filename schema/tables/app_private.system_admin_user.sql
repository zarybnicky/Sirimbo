CREATE TABLE app_private.system_admin_user (
    user_id bigint NOT NULL,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    created_by bigint DEFAULT public.current_user_id() NOT NULL
);

COMMENT ON TABLE app_private.system_admin_user IS '@omit all';
COMMENT ON COLUMN app_private.system_admin_user.user_id IS 'Globally privileged user allowed to manage tenants.';
COMMENT ON COLUMN app_private.system_admin_user.created_at IS 'Timestamp when the system administrator role was granted.';
COMMENT ON COLUMN app_private.system_admin_user.created_by IS 'User that granted the system administrator role.';

ALTER TABLE ONLY app_private.system_admin_user
    ADD CONSTRAINT system_admin_user_pkey PRIMARY KEY (user_id);
ALTER TABLE ONLY app_private.system_admin_user
    ADD CONSTRAINT system_admin_user_user_id_fkey FOREIGN KEY (user_id) REFERENCES public.users(id) ON DELETE CASCADE;
