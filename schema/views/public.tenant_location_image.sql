CREATE VIEW public.tenant_location_image WITH (security_invoker='true') AS
 SELECT tenant_id,
    location_id,
    file_id
   FROM public.location_image;

COMMENT ON VIEW public.tenant_location_image IS '@primaryKey tenant_id,location_id,file_id
@foreignKey (tenant_id,location_id) references tenant_location(tenant_id,id)|@fieldName location|@foreignFieldName images
@foreignKey (tenant_id,file_id) references file(tenant_id,id)|@fieldName file|@foreignFieldName locationImages
@omit create,update,delete
@simpleCollections only
@behavior -insert -update -delete';
COMMENT ON COLUMN public.tenant_location_image.tenant_id IS '@notNull';
COMMENT ON COLUMN public.tenant_location_image.location_id IS '@notNull';
COMMENT ON COLUMN public.tenant_location_image.file_id IS '@notNull';

GRANT SELECT ON TABLE public.tenant_location_image TO anonymous;
