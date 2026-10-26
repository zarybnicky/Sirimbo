CREATE FUNCTION public.competition_suggestions(event_ids bigint[], since date DEFAULT CURRENT_DATE, until date DEFAULT NULL::date, person_ids bigint[] DEFAULT NULL::bigint[], couple_id bigint DEFAULT NULL::bigint, cohort_id bigint DEFAULT NULL::bigint, registered_only boolean DEFAULT false) RETURNS TABLE(competition federated.competition, person public.person, couple public.couple, status text)
    LANGUAGE plpgsql STABLE
    AS $$
begin
  if event_ids is null then
    raise exception 'event_ids is required' using errcode = '22004';
  end if;
  return query
  select c, p, partnership, m.status
  from (
    select competition_id, local_person_id, local_couple_id, case
      when bool_or(matches.status = 'registered') then 'registered'
      when bool_or(matches.status = 'cancelled') then 'cancelled'
      else 'suggested'
    end as status
    from suggested_competition_matches(since, until, person_ids, couple_id, cohort_id, event_ids, registered_only)
      matches
    group by competition_id, local_person_id, local_couple_id
  ) m
  join federated.competition c on c.id = m.competition_id
  left join public.person p on p.id = m.local_person_id
  left join public.couple partnership on partnership.id = m.local_couple_id;
end;
$$;

COMMENT ON FUNCTION public.competition_suggestions(event_ids bigint[], since date, until date, person_ids bigint[], couple_id bigint, cohort_id bigint, registered_only boolean) IS '@behavior +list -connection';

GRANT ALL ON FUNCTION public.competition_suggestions(event_ids bigint[], since date, until date, person_ids bigint[], couple_id bigint, cohort_id bigint, registered_only boolean) TO anonymous;
