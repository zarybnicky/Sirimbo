create or replace function account_liabilities(a account, since timestamptz, until timestamptz) returns numeric(19,4) stable
begin atomic
  SELECT COALESCE(SUM(s), 0.0) FROM (
    SELECT 0 as s
    UNION
    SELECT COALESCE(sum(amount), 0.0) as s
     FROM posting
     JOIN transaction on transaction_id = transaction.id
    WHERE a.id = account_id AND amount < 0.0 AND effective_date >= since AND effective_date <= until
    GROUP BY account_id
  ) s;
end;

create or replace function account_assets(a account, since timestamptz, until timestamptz) returns numeric(19,4) stable
begin atomic
  SELECT COALESCE(SUM(s), 0.0) FROM (
    SELECT 0 as s
    UNION
    SELECT COALESCE(sum(amount), 0.0) as s
     FROM posting
     JOIN transaction on transaction_id = transaction.id
    WHERE a.id = account_id AND amount > 0.0 AND effective_date >= since AND effective_date <= until
    GROUP BY account_id
  ) s;
end;

grant all on function account_liabilities to anonymous;
grant all on function account_assets to anonymous;

alter table event_registration drop column if exists confirmed_at;
alter table event_registration drop column if exists status_time;
drop type if exists registration_time;


CREATE or replace FUNCTION public.create_event(INOUT info public.event, instances public.event_instance[], trainers public.event_trainer[], cohorts public.event_target_cohort[], registrations public.event_registration[]) RETURNS public.event
    LANGUAGE plpgsql
    AS $$
begin
  insert into event (name, summary, description, type, location_text, capacity, is_visible, is_public, is_locked, enable_notes)
  select info.name, info.summary, info.description, info.type, info.location_text, info.capacity, info.is_visible, info.is_public, info.is_locked, info.enable_notes
  returning * into info;

  insert into event_instance (event_id, since, until)
  select info.id, since, until from unnest(instances) i;

  insert into event_trainer (event_id, person_id, lessons_offered)
  select info.id, person_id, coalesce(lessons_offered, 0) from unnest(trainers) i;

  insert into event_target_cohort (event_id, cohort_id)
  select info.id, cohort_id from unnest(cohorts) i;

  insert into event_registration (event_id, person_id, couple_id)
  select info.id, person_id, couple_id from unnest(registrations) i;
end;
$$;

CREATE or replace FUNCTION public.upsert_event(INOUT info public.event, instances public.event_instance[], trainers public.event_trainer[], cohorts public.event_target_cohort[], registrations public.event_registration[]) RETURNS public.event
    LANGUAGE plpgsql
    AS $$
declare
  instance event_instance;
  trainer event_trainer;
  cohort event_target_cohort;
  registration event_registration;
begin
  if info.id is not null then
    update event set name=info.name, summary=info.summary, description=info.description, type=info.type, location_id=info.location_id, location_text=info.location_text, capacity=info.capacity, is_visible=info.is_visible, is_public=info.is_public, is_locked=info.is_locked, enable_notes=info.enable_notes where id=info.id;
  else
    insert into event (name, summary, description, type, location_id, location_text, capacity, is_visible, is_public, is_locked, enable_notes)
    values (info.name, info.summary, info.description, info.type, info.location_id, info.location_text, info.capacity, info.is_visible, info.is_public, info.is_locked, info.enable_notes)
    returning * into info;
  end if;

  foreach instance in array instances loop
    if instance.id is not null then
      if instance.since is null and instance.until is null then
        delete from event_instance where id=instance.id;
      else
        update event_instance set since=instance.since, until=instance.until where id=instance.id;
      end if;
    else
      insert into event_instance (event_id, since, until) values (info.id, instance.since, instance.until);
    end if;
  end loop;

  foreach trainer in array trainers loop
    if trainer.id is not null then
      if trainer.person_id is null then
        delete from event_trainer where id=trainer.id;
      else
        update event_trainer set lessons_offered=trainer.lessons_offered where id=trainer.id;
      end if;
    else
      insert into event_trainer (event_id, person_id, lessons_offered) values (info.id, trainer.person_id, coalesce(trainer.lessons_offered, 0));
    end if;
  end loop;

  foreach cohort in array cohorts loop
    if cohort.id is not null then
      if cohort.cohort_id is null then
        delete from event_target_cohort where id=cohort.id;
      end if;
    else
      insert into event_target_cohort (event_id, cohort_id) values (info.id, cohort.cohort_id);
    end if;
  end loop;

  foreach registration in array registrations loop
    if registration.id is not null then
      if registration.person_id is null and registration.couple_id is null then
        delete from event_registration where id=registration.id;
      else
        update event_registration
        set person_id=registration.person_id, couple_id=registration.couple_id
        where id=registration.id;
      end if;
    else
      insert into event_registration (event_id, person_id, couple_id)
      values (info.id, registration.person_id, registration.couple_id);
    end if;
  end loop;
end;
$$;
