CREATE FUNCTION public.tenant_couples(t public.tenant) RETURNS SETOF public.couple
    LANGUAGE sql STABLE
    BEGIN ATOMIC
 SELECT DISTINCT couple.id,
     couple.man_id,
     couple.woman_id,
     couple.since,
     couple.until,
     couple.created_at,
     couple.updated_at,
     couple.legacy_pary_id,
     couple.active_range,
     couple.status,
     couple.active
    FROM (public.couple
      JOIN public.tenant_membership ON (((couple.man_id = tenant_membership.person_id) OR (couple.woman_id = tenant_membership.person_id))))
   WHERE (couple.active AND tenant_membership.active AND (tenant_membership.tenant_id = (tenant_couples.t).id))
   ORDER BY couple.active_range;
END;

COMMENT ON FUNCTION public.tenant_couples(t public.tenant) IS '@simpleCollections only';

GRANT ALL ON FUNCTION public.tenant_couples(t public.tenant) TO anonymous;
