CREATE VIEW public.announcement AS
 SELECT up_id AS id,
    tenant_id,
    up_kdo AS author_id,
    up_nadpis AS title,
    up_text AS body,
    created_at,
    updated_at,
    scheduled_since,
    scheduled_until,
    up_lock AS is_locked,
    is_visible,
    sticky AS is_sticky
   FROM public.upozorneni;

COMMENT ON VIEW public.announcement IS '@primaryKey id
@foreignKey (tenant_id) references tenant (id)
@foreignKey (author_id) references users (id)
@simpleCollections both';
COMMENT ON COLUMN public.announcement.id IS '@hasDefault';
COMMENT ON COLUMN public.announcement.tenant_id IS '@hasDefault';
COMMENT ON COLUMN public.announcement.title IS '@notNull';
COMMENT ON COLUMN public.announcement.body IS '@notNull';
COMMENT ON COLUMN public.announcement.created_at IS '@notNull
@hasDefault';
COMMENT ON COLUMN public.announcement.is_locked IS '@notNull
@hasDefault';
COMMENT ON COLUMN public.announcement.is_sticky IS '@notNull
@hasDefault';

GRANT ALL ON TABLE public.announcement TO anonymous;
