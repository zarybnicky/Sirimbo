CREATE OR REPLACE FUNCTION public.detach_event_instance(
  p_instance_id     bigint,
  p_new_event_name  text   DEFAULT NULL
)
  RETURNS public.event
  LANGUAGE plpgsql
  SECURITY DEFINER
  SET search_path = pg_catalog, pg_temp
AS $$
DECLARE
  v_old_event_id bigint;
  v_new_event_id bigint;
  v_old_att jsonb;
  v_old_demands jsonb;
BEGIN
  -- Lock the instance and fetch old event_id
  SELECT ei.event_id
  INTO v_old_event_id
  FROM public.event_instance ei
  WHERE ei.id        = p_instance_id
    FOR UPDATE;

  IF v_old_event_id IS NULL THEN
    RAISE EXCEPTION 'detach_event_instance: instance % not found', p_instance_id;
  END IF;

  -- Also lock the parent event row (prevents concurrent edits during cloning)
  PERFORM 1
  FROM public.event e
  WHERE e.id = v_old_event_id
    FOR UPDATE;

  -- Snapshot attendance state for this instance so it can be restored after
  -- the legacy registration bridge is rebuilt for the cloned event.
  SELECT coalesce(jsonb_agg(jsonb_build_object(
    'reg_person_id', er.person_id,
    'reg_couple_id', er.couple_id,
    'attendee_person_id', eir.person_id,
    'registration_status', eir.registration_status,
    'status', eir.status,
    'attendance_note', eir.attendance_note,
    'attendance_created_at', eir.attendance_created_at,
    'attendance_updated_at', eir.attendance_updated_at
  )), '[]'::jsonb)
  INTO v_old_att
  FROM public.event_instance_registration eir
  JOIN public.event_registration er ON er.tenant_id = eir.tenant_id AND er.id = eir.legacy_registration_id AND er.event_id = v_old_event_id
  WHERE eir.instance_id = p_instance_id;

  SELECT coalesce(jsonb_agg(jsonb_build_object(
    'reg_person_id', er.person_id,
    'reg_couple_id', er.couple_id,
    'trainer_person_id', trainer.person_id,
    'lesson_count', demand.lesson_count,
    'created_at', demand.created_at
  )), '[]'::jsonb)
  INTO v_old_demands
  FROM public.event_lesson_demand demand
  JOIN public.event_instance_registration eir ON eir.id = demand.registration_id
  JOIN public.event_registration er ON er.id = eir.legacy_registration_id
  JOIN public.event_trainer trainer ON trainer.id = demand.trainer_id
  WHERE eir.instance_id = p_instance_id
    AND eir.parent_registration_id IS NULL;

  -- Reparenting synchronizes the new registrations before sweeping the old
  -- bridge rows, which would collide on the instance/person unit key.
  DELETE FROM public.event_instance_registration eir
  WHERE eir.instance_id = p_instance_id
    AND eir.event_id = v_old_event_id
    AND eir.legacy_registration_id IS NOT NULL;

  -- Clone the event (explicit column list, but short and stable in compact.sql)
  INSERT INTO public.event (
    name, location_text, description, capacity, files_legacy,
    updated_at, is_locked, is_visible, summary, is_public, enable_notes, tenant_id,
    type, location_id, created_at
  )
  SELECT
    COALESCE(p_new_event_name, e.name), e.location_text, e.description, e.capacity, e.files_legacy,
    now(), e.is_locked, e.is_visible, e.summary, e.is_public, e.enable_notes, e.tenant_id,
    e.type, e.location_id, now()
  FROM public.event e
  WHERE e.id = v_old_event_id
  RETURNING id INTO v_new_event_id;

  -- Copy event_target_cohort (natural key: cohort_id)
  INSERT INTO public.event_target_cohort (event_id, cohort_id, created_at, updated_at)
  SELECT v_new_event_id, etc.cohort_id, now(), now()
  FROM public.event_target_cohort etc
  WHERE etc.event_id = v_old_event_id
  ON CONFLICT (event_id, cohort_id) DO NOTHING;

  -- Copy event_trainer (natural key: person_id)
  INSERT INTO public.event_trainer (event_id, person_id, created_at, updated_at, lessons_offered)
  SELECT v_new_event_id, et.person_id, now(), now(), et.lessons_offered
  FROM public.event_trainer et
  WHERE et.event_id  = v_old_event_id
  ON CONFLICT (event_id, person_id) DO NOTHING;

  -- Re-point the instance (event_instance_trainer + any other (tenant_id, instance_id, event_id) dependents follow via FK cascade)
  UPDATE public.event_instance ei
  SET event_id   = v_new_event_id
  WHERE ei.id        = p_instance_id
    AND ei.event_id  = v_old_event_id;

  -- Copy registrations: map target_cohort_id by cohort_id (old_tc -> new_tc)
  INSERT INTO public.event_registration (
    event_id, target_cohort_id, couple_id, person_id, note, created_at
  )
  SELECT v_new_event_id, new_tc.id, er.couple_id, er.person_id, er.note, er.created_at
  FROM public.event_registration er
  LEFT JOIN public.event_target_cohort old_tc ON old_tc.tenant_id = er.tenant_id AND old_tc.id = er.target_cohort_id
  LEFT JOIN public.event_target_cohort new_tc ON new_tc.tenant_id = er.tenant_id AND new_tc.event_id  = v_new_event_id AND new_tc.cohort_id = old_tc.cohort_id
  WHERE er.event_id  = v_old_event_id
  ON CONFLICT (event_id, person_id, couple_id) DO NOTHING;

  INSERT INTO public.event_lesson_demand (
    trainer_id, registration_id, lesson_count, created_at, event_id
  )
  SELECT new_tr.id, new_eir.id, old.lesson_count, old.created_at, v_new_event_id
  FROM jsonb_to_recordset(v_old_demands) AS old(
    reg_person_id bigint,
    reg_couple_id bigint,
    trainer_person_id bigint,
    lesson_count integer,
    created_at timestamptz
  )
  JOIN public.event_trainer new_tr
    ON new_tr.event_id = v_new_event_id
   AND new_tr.person_id = old.trainer_person_id
  JOIN public.event_registration new_reg
    ON new_reg.event_id = v_new_event_id
   AND new_reg.person_id IS NOT DISTINCT FROM old.reg_person_id
   AND new_reg.couple_id IS NOT DISTINCT FROM old.reg_couple_id
  JOIN public.event_instance_registration new_eir
    ON new_eir.instance_id = p_instance_id
   AND new_eir.legacy_registration_id = new_reg.id
   AND new_eir.parent_registration_id IS NULL
  ON CONFLICT (registration_id, trainer_id) DO NOTHING;

  -- Restore per-person attendance on the newly generated bridge rows.
  UPDATE public.event_instance_registration eir
  SET registration_status = oa.registration_status,
      status = oa.status,
      attendance_note = oa.attendance_note
  FROM jsonb_to_recordset(v_old_att) AS oa(
    reg_person_id bigint,
    reg_couple_id bigint,
    attendee_person_id bigint,
    registration_status public.event_instance_registration_status,
    status public.attendance_type,
    attendance_note text
  )
  JOIN public.event_registration new_reg ON new_reg.event_id = v_new_event_id AND new_reg.person_id IS NOT DISTINCT FROM oa.reg_person_id AND new_reg.couple_id IS NOT DISTINCT FROM oa.reg_couple_id
  WHERE eir.instance_id     = p_instance_id
    AND eir.legacy_registration_id = new_reg.id
    AND eir.person_id IS NOT DISTINCT FROM oa.attendee_person_id;

  -- Restoring status/note correctly counts as an update for the new bridge row,
  -- so restore the original attendance audit lifecycle in a separate statement.
  UPDATE public.event_instance_registration eir
  SET attendance_created_at = oa.attendance_created_at,
      attendance_updated_at = oa.attendance_updated_at
  FROM jsonb_to_recordset(v_old_att) AS oa(
    reg_person_id bigint,
    reg_couple_id bigint,
    attendee_person_id bigint,
    attendance_created_at timestamptz,
    attendance_updated_at timestamptz
  )
  JOIN public.event_registration new_reg ON new_reg.event_id = v_new_event_id AND new_reg.person_id IS NOT DISTINCT FROM oa.reg_person_id AND new_reg.couple_id IS NOT DISTINCT FROM oa.reg_couple_id
  WHERE eir.instance_id = p_instance_id
    AND eir.legacy_registration_id = new_reg.id
    AND eir.person_id IS NOT DISTINCT FROM oa.attendee_person_id;

  return (select event from public.event where id = v_new_event_id);
END;
$$;

select verify_function('public.detach_event_instance');
grant execute on function detach_event_instance to anonymous;
