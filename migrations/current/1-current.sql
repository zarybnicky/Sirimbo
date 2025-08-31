create table if not exists public.tenant_settings (
    tenant_id bigint DEFAULT public.current_tenant_id() NOT NULL REFERENCES tenant(id) ON DELETE CASCADE primary key,
    settings jsonb not null
);

COMMENT ON TABLE tenant_settings is E'@simpleCollections only\n@omit create,delete';
GRANT ALL ON TABLE tenant_settings TO anonymous;
ALTER TABLE tenant_settings ENABLE ROW LEVEL SECURITY;

select app_private.drop_policies('public.tenant_settings');
CREATE POLICY current_tenant ON tenant_settings AS RESTRICTIVE USING ((tenant_id = (SELECT public.current_tenant_id())));
CREATE POLICY admin_own ON tenant_settings TO administrator USING (true) WITH CHECK (true);

insert into tenant_settings
select id, '{}'::jsonb from tenant
on conflict do nothing;


-- triggers - on create tenant, create tenant_settings


--!include functions/post_without_cache.sql
