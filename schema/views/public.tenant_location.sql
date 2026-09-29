CREATE VIEW public.tenant_location WITH (security_invoker='true') AS
 SELECT id,
    name,
    description,
    address,
    show_in_lists,
    tenant_id,
    created_at,
    updated_at,
    cover_image_id,
    is_public,
    long_name,
    latitude,
    longitude,
    ordering
   FROM public.location;

COMMENT ON VIEW public.tenant_location IS '@primaryKey id
@unique tenant_id,id
@foreignKey (tenant_id) references tenant(id)
@foreignKey (tenant_id,cover_image_id) references file(tenant_id,id)|@fieldName coverImage|@behavior -manyRelation:resource:list -manyRelation:resource:connection
@simpleCollections only
@behavior -insert -update -delete -query:resource:list -query:resource:connection -queryField:resource:connection';
COMMENT ON COLUMN public.tenant_location.name IS '@notNull';
COMMENT ON COLUMN public.tenant_location.description IS '@notNull';
COMMENT ON COLUMN public.tenant_location.show_in_lists IS '@notNull';
COMMENT ON COLUMN public.tenant_location.tenant_id IS '@notNull';
COMMENT ON COLUMN public.tenant_location.created_at IS '@notNull';
COMMENT ON COLUMN public.tenant_location.updated_at IS '@notNull';
COMMENT ON COLUMN public.tenant_location.ordering IS '@notNull';

GRANT SELECT ON TABLE public.tenant_location TO anonymous;
