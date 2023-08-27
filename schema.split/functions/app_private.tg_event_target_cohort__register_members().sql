CREATE FUNCTION app_private.tg_event_target_cohort__register_members() RETURNS trigger
    LANGUAGE plpgsql SECURITY DEFINER
    AS $$
begin
  insert into event_registration (event_id, person_id, target_cohort_id)
  select NEW.event_id, person_id, NEW.id from cohort_membership where cohort_membership.cohort_id = NEW.cohort_id and now() <@ active_range
  on conflict do nothing;
  return NEW;
end;
$$;



