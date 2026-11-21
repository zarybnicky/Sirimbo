CREATE TABLE public.tenant (
    id bigint NOT NULL,
    name text NOT NULL,
    origins text[] DEFAULT ARRAY[]::text[] NOT NULL,
    cz_ico text DEFAULT ''::text NOT NULL,
    cz_dic text DEFAULT ''::text NOT NULL,
    address public.address_domain,
    description text DEFAULT ''::text NOT NULL,
    bank_account text DEFAULT ''::text NOT NULL,
    csts_id integer,
    created_at timestamp with time zone DEFAULT now()
);

COMMENT ON TABLE public.tenant IS '@behavior +list -connection -insert -delete -singularRelation:resource:single -query:resource:connection';
COMMENT ON COLUMN public.tenant.created_at IS '@behavior -attribute:insert -attribute:update';

GRANT SELECT ON TABLE public.tenant TO anonymous;
GRANT ALL ON TABLE public.tenant TO system_admin;
GRANT UPDATE(name) ON TABLE public.tenant TO administrator;
GRANT UPDATE(cz_ico) ON TABLE public.tenant TO administrator;
GRANT UPDATE(cz_dic) ON TABLE public.tenant TO administrator;
GRANT UPDATE(address) ON TABLE public.tenant TO administrator;
GRANT UPDATE(description) ON TABLE public.tenant TO administrator;
GRANT UPDATE(bank_account) ON TABLE public.tenant TO administrator;
GRANT UPDATE(csts_id) ON TABLE public.tenant TO administrator;
ALTER TABLE public.tenant ENABLE ROW LEVEL SECURITY;

ALTER TABLE ONLY public.tenant
    ADD CONSTRAINT tenant_pkey PRIMARY KEY (id);

CREATE POLICY admin_all ON public.tenant TO administrator USING ((id = ( SELECT public.current_tenant_id() AS current_tenant_id)));
CREATE POLICY admin_view ON public.tenant FOR SELECT TO administrator USING ((id = ANY (( SELECT public.current_admin_tenant_ids() AS current_admin_tenant_ids)::bigint[])));
CREATE POLICY public_view ON public.tenant FOR SELECT TO anonymous USING ((id = ( SELECT public.current_tenant_id() AS current_tenant_id)));
CREATE POLICY system_admin_all ON public.tenant TO system_admin USING (true);
