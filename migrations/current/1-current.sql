-- Write your migration here

COMMENT ON TABLE public.payment IS '@omit create,delete
@simpleCollections both';

create or replace view cohort as
  select
    s_id as id,
    tenant_id,
    cohort_group as cohort_group_id,
    s_name as name,
    s_description as description,
    s_color_rgb as color_rgb,
    s_location as location,
    s_visible as is_visible,
    ordering
  from skupiny;

COMMENT ON VIEW cohort IS E'@primaryKey id
@foreignKey (tenant_id) references tenant (id)
@foreignKey (cohort_group_id) references cohort_group (id)
@simpleCollections only';
comment on column cohort.id is '@hasDefault';
comment on column cohort.name is '@notNull';
comment on column cohort.description is '@notNull';
comment on column cohort.color_rgb is '@notNull';
comment on column cohort.location is '@notNull
@hasDefault';
comment on column cohort.is_visible is '@notNull';
comment on column cohort.ordering is '@notNull
@hasDefault';
comment on column cohort.tenant_id is '@hasDefault';

COMMENT ON TABLE public.event_target_cohort IS E'@omit create,update,delete
@foreignKey (cohort_id) references cohort (id)
@simpleCollections only';
COMMENT ON CONSTRAINT event_target_cohort_cohort_id_fkey ON event_target_cohort IS E'@fieldName skupiny_id';

COMMENT ON TABLE public.cohort_membership IS E'@simpleCollections only
@foreignKey (cohort_id) references cohort (id)';
COMMENT ON CONSTRAINT cohort_membership_cohort_id_fkey ON cohort_membership IS E'@fieldName skupiny_id';

COMMENT ON TABLE public.cohort_subscription IS '@omit create,update,delete
@simpleCollections only
@foreignKey (cohort_id) references cohort (id)';
COMMENT ON CONSTRAINT cohort_subscription_cohort_id_fkey ON cohort_subscription IS E'@fieldName skupiny_id';

COMMENT ON TABLE public.upozorneni_skupiny IS E'@omit create,update,delete
@foreignKey (ups_id_skupina) references cohort (id)';
