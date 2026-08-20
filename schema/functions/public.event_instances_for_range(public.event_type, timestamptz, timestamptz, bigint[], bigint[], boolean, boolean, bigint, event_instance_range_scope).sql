CREATE FUNCTION public.event_instances_for_range(only_type public.event_type, start_range timestamp with time zone, end_range timestamp with time zone DEFAULT NULL::timestamp with time zone, trainer_ids bigint[] DEFAULT NULL::bigint[], participant_ids bigint[] DEFAULT NULL::bigint[], only_mine boolean DEFAULT false, any_parent boolean DEFAULT true, parent_id bigint DEFAULT NULL::bigint, scope public.event_instance_range_scope DEFAULT NULL::public.event_instance_range_scope) RETURNS SETOF public.event_instance
    LANGUAGE sql STABLE
    AS $_$
  with mine as (
    select instance_id from event_instance_registration
    where person_id = any (current_person_ids()) and registration_status = 'active'
    union all
    select instance_id from event_instance_trainer
    where person_id = any (current_person_ids())
  )
  select i.*
  from event_instance i
  cross join (values (coalesce($9, case
    when only_mine then 'mine'
    when not any_parent then 'top_level'
    else 'all'
  end::event_instance_range_scope))) args(scope)
  where i.tenant_id = current_tenant_id()
    and (only_type is null or i.type = only_type)
    and case
      when $8 is not null then i.parent_id = $8
        and (args.scope <> 'mine' or i.id in (select instance_id from mine))
      when args.scope = 'all' then true
      when args.scope = 'top_level' then i.parent_id is null
      when args.scope = 'mine' then i.id in (select instance_id from mine)
      when args.scope = 'relevant' then i.parent_id is null
        or i.id in (select instance_id from mine)
        or i.parent_id in (select instance_id from mine)
    end
    and i.since < coalesce(end_range, 'infinity'::timestamptz)
    and i.until > start_range
    and (trainer_ids is null
      or exists (select 1 from event_instance_trainer where instance_id = i.id and person_id = any (trainer_ids)))
    and (participant_ids is null
      or exists (select 1 from event_instance_registration where instance_id = i.id and person_id = any (participant_ids) and registration_status = 'active'))
  ;
$_$;

COMMENT ON FUNCTION public.event_instances_for_range(only_type public.event_type, start_range timestamp with time zone, end_range timestamp with time zone, trainer_ids bigint[], participant_ids bigint[], only_mine boolean, any_parent boolean, parent_id bigint, scope public.event_instance_range_scope) IS '@simpleCollections only';

GRANT ALL ON FUNCTION public.event_instances_for_range(only_type public.event_type, start_range timestamp with time zone, end_range timestamp with time zone, trainer_ids bigint[], participant_ids bigint[], only_mine boolean, any_parent boolean, parent_id bigint, scope public.event_instance_range_scope) TO anonymous;
