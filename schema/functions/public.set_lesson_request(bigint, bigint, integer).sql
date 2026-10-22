CREATE FUNCTION public.set_lesson_request(registration_id bigint, trainer_id bigint, lesson_count integer) RETURNS public.event_lesson_request
    LANGUAGE plpgsql STRICT SECURITY DEFINER
    SET search_path TO 'pg_catalog', 'public', 'pg_temp'
    AS $_$
declare
  registration event_registration;
  instance event;
  is_manager boolean;
  is_self boolean;
begin
  select * into registration
  from event_registration
  where id = $1;

  if not found then
    raise exception 'REGISTRATION_NOT_FOUND' using errcode = '28000';
  end if;

  select * into instance
  from event
  where id = registration.event_id and tenant_id = current_tenant_id();

  if not found then
    raise exception 'INSTANCE_NOT_FOUND' using errcode = '28000';
  end if;

  is_self := registration.person_id = any(current_person_ids())
    or registration.couple_id = any(current_couple_ids());
  is_manager := pg_has_role(
    coalesce(nullif(current_setting('role'), 'none'), session_user), 'administrator', 'member'
  ) or (
    pg_has_role(
      coalesce(nullif(current_setting('role'), 'none'), session_user), 'trainer', 'member'
    ) and app_private.can_trainer_edit_event(instance.id)
  );

  if not coalesce(is_self, false) and not coalesce(is_manager, false) then
    raise exception 'ACCESS_DENIED' using errcode = '42501';
  end if;

  if instance.is_locked and not is_manager then
    raise exception 'NOT_ALLOWED' using errcode = '28000';
  end if;

  return app_private.set_lesson_request(registration, $2, $3);
end;
$_$;

GRANT ALL ON FUNCTION public.set_lesson_request(registration_id bigint, trainer_id bigint, lesson_count integer) TO anonymous;
