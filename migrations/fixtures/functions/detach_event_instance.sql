CREATE OR REPLACE FUNCTION public.detach_event_instance(
  p_instance_id     bigint,
  p_tenant_id       bigint DEFAULT public.current_tenant_id(),
  p_new_event_name  text   DEFAULT NULL
)
  RETURNS event
  LANGUAGE plpgsql
AS $$
DECLARE
  v_old_event_id bigint;
  v_new_event_id bigint;
  v_new_event event;
  v_old_att jsonb;
BEGIN
  -- Lock the instance and fetch old event_id
  SELECT ei.event_id
  INTO v_old_event_id
  FROM public.event_instance ei
  WHERE ei.tenant_id = p_tenant_id
    AND ei.id        = p_instance_id
    FOR UPDATE;

  IF v_old_event_id IS NULL THEN
    RAISE EXCEPTION 'detach_event_instance: instance % not found in tenant %', p_instance_id, p_tenant_id;
  END IF;

  -- Also lock the parent event row (prevents concurrent edits during cloning)
  PERFORM 1
  FROM public.event e
  WHERE e.tenant_id = p_tenant_id
    AND e.id        = v_old_event_id
    FOR UPDATE;

  -- Snapshot attendance state for this instance so it can be restored after
  -- the legacy registration bridge is rebuilt for the cloned event.
  SELECT coalesce(jsonb_agg(jsonb_build_object(
    'reg_person_id', er.person_id,
    'reg_couple_id', er.couple_id,
    'attendee_person_id', eir.person_id,
    'status', eir.status,
    'note', eir.note,
    'attendance_created_at', eir.attendance_created_at,
    'attendance_updated_at', eir.attendance_updated_at
  )), '[]'::jsonb)
  INTO v_old_att
  FROM public.event_instance_registration eir
  JOIN public.event_registration er ON er.tenant_id = eir.tenant_id AND er.id = eir.legacy_registration_id AND er.event_id = v_old_event_id
  WHERE eir.tenant_id   = p_tenant_id
    AND eir.instance_id = p_instance_id
    AND eir.person_id   IS NOT NULL;

  -- Reparenting synchronizes the new registrations before sweeping the old
  -- bridge rows, which would collide on the instance/person unit key.
  DELETE FROM public.event_instance_registration eir
  WHERE eir.tenant_id = p_tenant_id
    AND eir.instance_id = p_instance_id
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
  WHERE e.tenant_id = p_tenant_id
    AND e.id        = v_old_event_id
  RETURNING id INTO v_new_event_id;

  -- Copy event_target_cohort (natural key: cohort_id)
  INSERT INTO public.event_target_cohort (tenant_id, event_id, cohort_id, created_at, updated_at)
  SELECT p_tenant_id, v_new_event_id, etc.cohort_id, now(), now()
  FROM public.event_target_cohort etc
  WHERE etc.tenant_id = p_tenant_id
    AND etc.event_id  = v_old_event_id
  ON CONFLICT (event_id, cohort_id) DO NOTHING;

  -- Copy event_trainer (natural key: person_id)
  INSERT INTO public.event_trainer (tenant_id, event_id, person_id, created_at, updated_at, lessons_offered)
  SELECT p_tenant_id, v_new_event_id, et.person_id, now(), now(), et.lessons_offered
  FROM public.event_trainer et
  WHERE et.tenant_id = p_tenant_id
    AND et.event_id  = v_old_event_id
  ON CONFLICT (event_id, person_id) DO NOTHING;

  -- Re-point the instance (event_instance_trainer + any other (tenant_id, instance_id, event_id) dependents follow via FK cascade)
  UPDATE public.event_instance ei
  SET event_id   = v_new_event_id
  WHERE ei.tenant_id = p_tenant_id
    AND ei.id        = p_instance_id
    AND ei.event_id  = v_old_event_id;

  -- Copy registrations: map target_cohort_id by cohort_id (old_tc -> new_tc)
  INSERT INTO public.event_registration (
    tenant_id, event_id, target_cohort_id, couple_id, person_id, note, created_at
  )
  SELECT p_tenant_id, v_new_event_id, new_tc.id, er.couple_id, er.person_id, er.note, er.created_at
  FROM public.event_registration er
  LEFT JOIN public.event_target_cohort old_tc ON old_tc.tenant_id = er.tenant_id AND old_tc.id = er.target_cohort_id
  LEFT JOIN public.event_target_cohort new_tc ON new_tc.tenant_id = er.tenant_id AND new_tc.event_id  = v_new_event_id AND new_tc.cohort_id = old_tc.cohort_id
  WHERE er.tenant_id = p_tenant_id
    AND er.event_id  = v_old_event_id
  ON CONFLICT (event_id, person_id, couple_id) DO NOTHING;

  -- Copy lesson demand:
  -- - trainer maps by person_id (old event_trainer -> new event_trainer)
  -- - registration maps by (person_id, couple_id) to new event_registration
  INSERT INTO public.event_lesson_demand (
    tenant_id, trainer_id, registration_id, lesson_count, created_at, event_id
  )
  SELECT p_tenant_id, new_tr.id, new_reg.id, d.lesson_count, d.created_at, v_new_event_id
  FROM public.event_lesson_demand d
  JOIN public.event_trainer old_tr ON old_tr.tenant_id = p_tenant_id AND old_tr.id = d.trainer_id AND old_tr.event_id  = v_old_event_id
  JOIN public.event_trainer new_tr ON new_tr.tenant_id = p_tenant_id AND new_tr.event_id  = v_new_event_id AND new_tr.person_id = old_tr.person_id
  JOIN public.event_registration old_reg ON old_reg.tenant_id = p_tenant_id AND old_reg.id = d.registration_id AND old_reg.event_id  = v_old_event_id
  JOIN public.event_registration new_reg ON new_reg.tenant_id = p_tenant_id AND new_reg.event_id = v_new_event_id AND new_reg.person_id IS NOT DISTINCT FROM old_reg.person_id AND new_reg.couple_id IS NOT DISTINCT FROM old_reg.couple_id
  WHERE d.tenant_id = p_tenant_id
    AND d.event_id  = v_old_event_id
  ON CONFLICT (registration_id, trainer_id) DO NOTHING;

  -- Restore attendance status/note on the newly generated attendance rows for the detached instance.
  UPDATE public.event_instance_registration eir
  SET status = oa.status, note = oa.note
  FROM jsonb_to_recordset(v_old_att) AS oa(
    reg_person_id bigint,
    reg_couple_id bigint,
    attendee_person_id bigint,
    status public.attendance_type,
    note text,
    attendance_created_at timestamptz,
    attendance_updated_at timestamptz
  )
  JOIN public.event_registration new_reg ON new_reg.tenant_id = p_tenant_id AND new_reg.event_id = v_new_event_id AND new_reg.person_id IS NOT DISTINCT FROM oa.reg_person_id AND new_reg.couple_id IS NOT DISTINCT FROM oa.reg_couple_id
  WHERE eir.tenant_id       = p_tenant_id
    AND eir.instance_id     = p_instance_id
    AND eir.legacy_registration_id = new_reg.id
    AND eir.person_id       = oa.attendee_person_id;

  -- Restoring status/note correctly counts as an update for the new bridge row,
  -- so restore the original attendance audit lifecycle in a separate statement.
  UPDATE public.event_instance_registration eir
  SET attendance_created_at = oa.attendance_created_at,
      attendance_updated_at = oa.attendance_updated_at
  FROM jsonb_to_recordset(v_old_att) AS oa(
    reg_person_id bigint,
    reg_couple_id bigint,
    attendee_person_id bigint,
    status public.attendance_type,
    note text,
    attendance_created_at timestamptz,
    attendance_updated_at timestamptz
  )
  JOIN public.event_registration new_reg ON new_reg.tenant_id = p_tenant_id AND new_reg.event_id = v_new_event_id AND new_reg.person_id IS NOT DISTINCT FROM oa.reg_person_id AND new_reg.couple_id IS NOT DISTINCT FROM oa.reg_couple_id
  WHERE eir.tenant_id = p_tenant_id
    AND eir.instance_id = p_instance_id
    AND eir.legacy_registration_id = new_reg.id
    AND eir.person_id = oa.attendee_person_id;

  select * into v_new_event from public.event where id = v_new_event_id;
  RETURN v_new_event;
END;
$$;

grant all on function detach_event_instance to trainer;
select verify_function('detach_event_instance');
