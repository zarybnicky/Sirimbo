--! Previous: sha1:69ede70235bd94cc2f045b45b9a126fd72d3763d
--! Hash: sha1:a1274c434aa8c82667993491ab95991c961ac7d3

--! split: 1-current.sql
CREATE or replace FUNCTION app_private.create_latest_lesson_payments() RETURNS SETOF payment
  LANGUAGE plpgsql
AS $$
begin
  perform set_config('jwt.claims.tenant_id', '2', true);
  perform set_config('jwt.claims.my_tenant_ids', '[2]', true);
  if not (select row_security_active('event')) then
    set local role to administrator;
  end if;

  return query WITH created AS (
    SELECT p.*
    FROM event_instance ei
      JOIN event e ON e.id = ei.event_id
      JOIN LATERAL create_event_instance_payment(ei) p ON true
    WHERE e.type = 'lesson'
      AND NOT ei.is_cancelled
      AND ei.since < now()
      AND NOT EXISTS (
        SELECT 1
        FROM payment p
        WHERE p.event_instance_id = ei.id AND p.status = 'paid'
      )
      AND p.status in ('unpaid', 'tentative')
  ),
  unpaid AS (
    UPDATE payment p
      SET status = 'unpaid'
      FROM created
      WHERE p.id = created.id
      RETURNING p.*
  )
  SELECT p.*
  FROM unpaid
    CROSS JOIN LATERAL resolve_payment_with_credit(unpaid.*) p
  WHERE p IS NOT NULL;
end;
$$;

select verify_function('app_private.create_latest_lesson_payments');

CREATE or replace FUNCTION app_private.tg_account_balances__update() RETURNS trigger
  LANGUAGE plpgsql SECURITY DEFINER
  SET search_path TO 'pg_catalog', 'public', 'pg_temp'
AS $$
BEGIN
  PERFORM graphile_worker.add_job(
    'refresh_account_balances',
    job_key := 'refresh_account_balances'
  );
  return null;
END
$$;

CREATE or replace FUNCTION app_private.tg_cohort_membership__on_status()
  RETURNS trigger LANGUAGE plpgsql
  security definer
  SET search_path = pg_catalog, public, app_private
  AS $$
begin
  -- Only react to status transitions (or INSERT).
  IF TG_OP <> 'INSERT' AND NEW.status IS NOT DISTINCT FROM OLD.status THEN
    RETURN NEW;
  END IF;

  if NEW.status = 'expired' then
    WITH affected AS (
      SELECT DISTINCT er.id AS registration_id
      FROM event_target_cohort etc
      JOIN event_registration er ON er.event_id = etc.event_id
      WHERE etc.cohort_id = NEW.cohort_id
        AND (er.person_id = NEW.person_id OR (
          er.couple_id IS NOT NULL AND EXISTS (
            SELECT 1 FROM couple c WHERE c.id = er.couple_id AND NEW.person_id IN (c.man_id, c.woman_id))
          )
        )
    ),
    future_att AS (
      -- Capture future attendance rows with their pre-update status
      SELECT ea.id, ea.registration_id,
             (ea.status IN ('unknown', 'not-excused', 'cancelled')) AS will_be_cancelled
      FROM event_attendance ea
      JOIN event_instance ei ON ei.id = ea.instance_id
      JOIN affected a ON a.registration_id = ea.registration_id
      WHERE ei.since > NEW.until
    ),
    upd AS (
      UPDATE event_attendance ea
        SET status = 'cancelled'
        FROM future_att fa
        WHERE ea.id = fa.id
          AND ea.status IN ('unknown', 'not-excused')
        RETURNING ea.registration_id
    ),
    deletable AS (
      -- Delete registrations where:
      --   (a) all future attendances will be cancelled (pre-update check avoids CTE
      --       visibility issue with upd), AND
      --   (b) there are no non-cancelled past/present attendance rows
      SELECT fa.registration_id
      FROM future_att fa
      GROUP BY fa.registration_id
      HAVING count(*) > 0
         AND bool_and(fa.will_be_cancelled)
         AND NOT EXISTS (
           SELECT 1 FROM event_attendance ea2
           JOIN event_instance ei2 ON ei2.id = ea2.instance_id
           WHERE ea2.registration_id = fa.registration_id
             AND ei2.since <= NEW.until
             AND ea2.status NOT IN ('unknown', 'cancelled')
         )
    )
    DELETE FROM event_registration er
      USING deletable d
    WHERE er.id = d.registration_id;
  elsif NEW.status = 'active' then
    perform app_private.register_new_cohort_member_to_events(NEW);
    -- TODO: add payments
  end if;
  return NEW;
end;
$$;

grant all on function app_private.tg_cohort_membership__on_status to trainer;
select verify_function('app_private.tg_cohort_membership__on_status', 'cohort_membership');

DROP TRIGGER IF EXISTS _500_on_status ON public.cohort_membership;
CREATE TRIGGER _500_on_status AFTER INSERT OR UPDATE ON public.cohort_membership FOR EACH ROW EXECUTE FUNCTION app_private.tg_cohort_membership__on_status();

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

  -- Snapshot attendance state for this instance so we can restore status/note later
  CREATE TEMP TABLE _old_att (
     reg_person_id        bigint,
     reg_couple_id        bigint,
     attendee_person_id   bigint NOT NULL,
     status               public.attendance_type NOT NULL,
     note                 text
  ) ON COMMIT DROP;

  PERFORM plpgsql_check_pragma('disable:check');
  INSERT INTO _old_att (reg_person_id, reg_couple_id, attendee_person_id, status, note)
  SELECT er.person_id, er.couple_id, ea.person_id, ea.status, ea.note
  FROM public.event_attendance ea
  JOIN public.event_registration er ON er.tenant_id = ea.tenant_id AND er.id = ea.registration_id AND er.event_id  = ea.event_id
  WHERE ea.tenant_id   = p_tenant_id
    AND ea.instance_id = p_instance_id
    AND ea.event_id    = v_old_event_id;
  PERFORM plpgsql_check_pragma('enable:check');

  -- Critical: delete instance attendance BEFORE changing instance.event_id
  DELETE FROM public.event_attendance ea
  WHERE ea.tenant_id   = p_tenant_id
    AND ea.instance_id = p_instance_id
    AND ea.event_id    = v_old_event_id;

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

  -- Explicitly create attendance rows for the detached instance.
  -- tg_event_registration__create_attendance fires on registration INSERT but
  -- queries event_instance by event_id; since the instance was re-pointed via
  -- UPDATE (not INSERT) the tg_event_instance__create_attendance trigger never
  -- fired. Insert directly to guarantee coverage regardless of trigger ordering.
  INSERT INTO public.event_attendance (tenant_id, registration_id, instance_id, person_id)
  SELECT p_tenant_id, new_reg.id, p_instance_id, pid
  FROM public.event_registration new_reg
  CROSS JOIN LATERAL app_private.event_registration_person_ids(new_reg) AS pid
  WHERE new_reg.tenant_id = p_tenant_id
    AND new_reg.event_id  = v_new_event_id
  ON CONFLICT (registration_id, instance_id, person_id) DO NOTHING;

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

  -- Restore attendance status/note on the newly generated attendance rows for the detached instance
  PERFORM plpgsql_check_pragma('disable:check');
  UPDATE public.event_attendance ea
  SET status = oa.status, note = oa.note
  FROM _old_att oa
  JOIN public.event_registration new_reg ON new_reg.tenant_id = p_tenant_id AND new_reg.event_id = v_new_event_id AND new_reg.person_id IS NOT DISTINCT FROM oa.reg_person_id AND new_reg.couple_id IS NOT DISTINCT FROM oa.reg_couple_id
  WHERE ea.tenant_id       = p_tenant_id
    AND ea.instance_id     = p_instance_id
    AND ea.event_id        = v_new_event_id
    AND ea.registration_id = new_reg.id
    AND ea.person_id       = oa.attendee_person_id;
  PERFORM plpgsql_check_pragma('enable:check');

  select * into v_new_event from event where id = v_new_event_id;
  RETURN v_new_event;
END;
$$;

grant all on function detach_event_instance to trainer;


CREATE OR REPLACE FUNCTION public.sync_cohort_memberships(person_id bigint, cohort_ids bigint[]) RETURNS void
    LANGUAGE sql
    AS $_$
  update cohort_membership set until = now(), status = 'expired'
  where status = 'active' and person_id = $1 and cohort_id <> all (cohort_ids);

  insert into cohort_membership (status, since, person_id, cohort_id)
  select 'active', NOW(), $1, new_cohort_id
  from unnest(cohort_ids) as x(new_cohort_id)
  where not exists (select 1 from cohort_membership where status = 'active' and person_id = $1 and cohort_id = new_cohort_id);
$_$;

GRANT ALL ON FUNCTION public.sync_cohort_memberships(person_id bigint, cohort_ids bigint[]) TO administrator;
