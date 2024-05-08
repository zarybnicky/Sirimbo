CREATE VIEW public.cohort AS
 SELECT skupiny.s_id AS id,
    skupiny.tenant_id,
    skupiny.cohort_group AS cohort_group_id,
    skupiny.s_name AS name,
    skupiny.s_description AS description,
    skupiny.s_color_rgb AS color_rgb,
    skupiny.s_location AS location,
    skupiny.s_visible AS is_visible,
    skupiny.ordering
   FROM public.skupiny;

COMMENT ON VIEW public.cohort IS '@primaryKey id
@foreignKey (tenant_id) references tenant (id)
@foreignKey (cohort_group_id) references cohort_group (id)
@simpleCollections only';
COMMENT ON COLUMN public.cohort.id IS '@hasDefault';
COMMENT ON COLUMN public.cohort.tenant_id IS '@hasDefault';
COMMENT ON COLUMN public.cohort.name IS '@notNull';
COMMENT ON COLUMN public.cohort.description IS '@notNull';
COMMENT ON COLUMN public.cohort.color_rgb IS '@notNull';
COMMENT ON COLUMN public.cohort.location IS '@notNull
@hasDefault';
COMMENT ON COLUMN public.cohort.is_visible IS '@notNull';
COMMENT ON COLUMN public.cohort.ordering IS '@notNull
@hasDefault';

GRANT ALL ON TABLE public.cohort TO anonymous;


