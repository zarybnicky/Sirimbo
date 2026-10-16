--! Previous: sha1:9f6ba8bd931ff1ae70f60790230b9ef0e2cd25af
--! Hash: sha1:ab76d81ce4dd572d73f54f0f6768246ac629da11

--! split: 1-current.sql
lock table crawler.frontier in exclusive mode;

-- A partial WDSF response can introduce more dances into an existing round.
-- Keep its dance rows attached when the program expands.
ALTER TABLE federated.round_dance
  DROP CONSTRAINT round_dance_round_id_dance_program_id_fkey,
  ADD CONSTRAINT round_dance_round_id_dance_program_id_fkey
    FOREIGN KEY (round_id, dance_program_id)
    REFERENCES federated.competition_round (id, dance_program_id)
    ON UPDATE CASCADE ON DELETE CASCADE;

INSERT INTO federated.dance (code, name, discipline) VALUES
  ('BO', 'Bolero', 'rhythm'),
  ('MA', 'Mambo', 'rhythm'),
  ('SG', 'Swing', 'rhythm')
ON CONFLICT (code) DO NOTHING;

COMMENT ON TABLE public.announcement_attachment IS '@behavior -insert -update -delete -query:resource:connection -query:resource:list -query:resource:single';
COMMENT ON TABLE public.article_attachment IS '@behavior -insert -update -delete -query:resource:connection -query:resource:list -query:resource:single';

COMMENT ON TABLE public.location_image IS '@simpleCollections only
@behavior -insert -update -delete -query:resource:connection -query:resource:list';

COMMENT ON VIEW public.tenant_location_image IS '@primaryKey tenant_id,location_id,file_id
@foreignKey (tenant_id,location_id) references tenant_location(tenant_id,id)|@fieldName location|@foreignFieldName images
@foreignKey (tenant_id,file_id) references file(tenant_id,id)|@fieldName file|@foreignFieldName locationImages
@simpleCollections only
@behavior -insert -update -delete -query:resource:connection -query:resource:list';

COMMENT ON VIEW public.tenant_location IS '@primaryKey id
@unique tenant_id,id|@behavior -query:resource:single
@foreignKey (tenant_id) references tenant(id)
@foreignKey (tenant_id,cover_image_id) references file(tenant_id,id)|@fieldName coverImage|@behavior -manyRelation:resource:list -manyRelation:resource:connection
@simpleCollections only
@behavior -insert -update -delete -query:resource:list -query:resource:connection -queryField:resource:connection';

COMMENT ON TABLE federated.person IS '@name federation_person
@behavior -insert -update -delete -list
@ref people via:public.person_federation_link;public.person behavior:"+list -connection"';
COMMENT ON TABLE federated.person_club_membership IS '@name federation_person_club_membership
@behavior -insert -update -delete -list -query:resource:connection -query:resource:list -query:resource:single';
COMMENT ON TABLE federated.person_license IS '@name federation_person_license
@behavior -insert -update -delete -list -query:resource:connection -query:resource:list -query:resource:single';
COMMENT ON TABLE federated.event IS '@name federation_event
@behavior -insert -update -delete -list';
COMMENT ON TABLE federated.event_official IS '@name federation_event_official
@behavior -insert -update -delete -list -query:resource:connection -query:resource:list -query:resource:single';
COMMENT ON TABLE federated.competitor IS '@name federation_competitor
@behavior -insert -update -delete -list
@ref people via:federated.competitor_component;federated.person;public.person_federation_link;public.person behavior:"+list -connection"
@ref couples via:public.couple_federation_link;public.couple
@ref competitions via:federated.competition_participation;federated.competition';
COMMENT ON TABLE federated.competitor_component IS '@name federation_competitor_component
@behavior -insert -update -delete -list -query:resource:connection -query:resource:list -query:resource:single';
COMMENT ON TABLE federated.competitor_category_progress IS '@name federation_competitor_category_progress
@behavior -insert -update -delete -list -query:resource:connection -query:resource:list -query:resource:single';
COMMENT ON TABLE federated.competitor_club_affiliation IS '@name federation_competitor_club_affiliation
@behavior -insert -update -delete -list -query:resource:connection -query:resource:list -query:resource:single';
COMMENT ON TABLE federated.round_dance IS '@name competition_round_dance
@behavior -insert -update -delete -list -query:resource:connection -query:resource:list -query:resource:single
@ref dance singular via:federated.dance_program_dance;federated.dance';
COMMENT ON TABLE crawler.frontier IS '@name crawler_target
@behavior -insert -update -delete -list';
COMMENT ON VIEW crawler.frontier_failure IS '@name crawler_target_failure
@behavior -insert -update -delete -list';
COMMENT ON VIEW crawler.frontier_fetch_job IS '@name crawler_fetch_job
@behavior -insert -update -delete -list';
COMMENT ON TABLE crawler.json_response IS '@name crawler_response
@behavior -insert -update -delete -list';
COMMENT ON TABLE crawler.json_response_cache IS '@name crawler_response_cache
@behavior -insert -update -delete -list';
COMMENT ON TABLE crawler.incremental_ranges IS '@name crawler_incremental_range
@behavior -insert -update -delete -list';
COMMENT ON TABLE crawler.rate_limit_rule IS '@name crawler_rate_limit_rule
@behavior -insert -update -delete -list';
COMMENT ON TABLE federated.category IS '@behavior -insert -update -delete +list -connection';
COMMENT ON TABLE federated.competition IS '@behavior -insert -update -delete -list';
COMMENT ON TABLE federated.competition_entry IS '@behavior -insert -update -delete -list -query:resource:connection -query:resource:list -query:resource:single';
COMMENT ON TABLE federated.competition_official IS '@behavior -insert -update -delete -list -query:resource:connection -query:resource:list -query:resource:single';
COMMENT ON TABLE federated.competition_result IS '@behavior -insert -update -delete -list -query:resource:connection -query:resource:list -query:resource:single';
COMMENT ON TABLE federated.competition_round IS '@behavior -insert -update -delete -list -query:resource:connection -query:resource:list';
COMMENT ON TABLE federated.competition_round_judge IS '@behavior -insert -update -delete -list -query:resource:connection -query:resource:list -query:resource:single';
COMMENT ON TABLE federated.competition_round_result IS '@behavior -insert -update -delete -list -query:resource:connection -query:resource:list -query:resource:single';
COMMENT ON TABLE federated.dance IS '@behavior -insert -update -delete +list -connection';
COMMENT ON TABLE federated.dance_program IS '@behavior -insert -update -delete +list -connection';
COMMENT ON TABLE federated.dance_program_dance IS '@behavior -insert -update -delete +list -connection';
COMMENT ON TABLE federated.federation IS '@behavior -insert -update -delete +list -connection';
COMMENT ON TABLE federated.federation_club IS '@behavior -insert -update -delete -list';
COMMENT ON TABLE federated.judge_score IS '@behavior -insert -update -delete -list -query:resource:connection -query:resource:list -query:resource:single';
COMMENT ON TABLE federated.ranklist IS '@behavior -insert -update -delete -list';
COMMENT ON TABLE federated.ranklist_entry IS '@behavior -insert -update -delete -list -query:resource:connection -query:resource:list -query:resource:single';
COMMENT ON TABLE federated.ranklist_snapshot IS '@behavior -insert -update -delete -list -query:resource:connection -query:resource:list -query:resource:single';
COMMENT ON FUNCTION crawler.reserve_request(text) IS '@behavior -*';
COMMENT ON FUNCTION crawler.store_response(bigint, text, integer, text, jsonb, crawler.fetch_status) IS '@behavior -*';

COMMENT ON CONSTRAINT competition_federation_id_key ON federated.competition IS '@behavior -query:resource:single';
COMMENT ON CONSTRAINT competition_id_category_id_key ON federated.competition IS '@behavior -query:resource:single';
COMMENT ON CONSTRAINT competition_id_event_id_key ON federated.competition IS '@behavior -query:resource:single';
COMMENT ON CONSTRAINT competition_round_id_competition_id_key ON federated.competition_round IS '@behavior -query:resource:single';
COMMENT ON CONSTRAINT competition_round_id_dance_program_id_key ON federated.competition_round IS '@behavior -query:resource:single';
COMMENT ON CONSTRAINT event_federation_id_key ON federated.event IS '@behavior -query:resource:single';
COMMENT ON CONSTRAINT federation_club_federation_id_key ON federated.federation_club IS '@behavior -query:resource:single';
COMMENT ON CONSTRAINT round_dance_round_id_dance_order_dance_code_key ON federated.round_dance IS '@behavior -query:resource:single';

COMMENT ON CONSTRAINT cohort_group_tenant_id_id_key ON public.cohort_group IS '@behavior -query:resource:single -constraint:resource:update -constraint:resource:delete';
COMMENT ON CONSTRAINT cohort_tenant_id_id_key ON public.cohort IS '@behavior -query:resource:single -constraint:resource:update -constraint:resource:delete';
COMMENT ON CONSTRAINT event_instance_tenant_id_id_key ON public.event_instance IS '@behavior -query:resource:single -constraint:resource:update -constraint:resource:delete';
COMMENT ON CONSTRAINT event_series_tenant_id_id_key ON public.event_series IS '@behavior -query:resource:single -constraint:resource:update -constraint:resource:delete';
COMMENT ON CONSTRAINT payment_tenant_id_id_key ON public.payment IS '@behavior -query:resource:single -constraint:resource:update -constraint:resource:delete';
COMMENT ON CONSTRAINT tenant_location_tenant_id_id_key ON public.location IS '@behavior -query:resource:single -constraint:resource:update -constraint:resource:delete';
COMMENT ON CONSTRAINT transaction_tenant_id_id_key ON public.transaction IS '@behavior -query:resource:single -constraint:resource:update -constraint:resource:delete';

GRANT USAGE ON SCHEMA crawler TO system_admin;
GRANT SELECT ON ALL TABLES IN SCHEMA crawler TO system_admin;
REVOKE EXECUTE ON ALL FUNCTIONS IN SCHEMA crawler FROM PUBLIC, system_admin;

COMMENT ON CONSTRAINT frontier_last_response_id_fkey ON crawler.frontier IS '@foreignFieldName targetsWithLatestResponse';
COMMENT ON CONSTRAINT frontier_last_successful_response_id_fkey ON crawler.frontier IS '@foreignFieldName targetsWithLastSuccessfulResponse';
COMMENT ON CONSTRAINT json_response_content_hash_fkey ON crawler.json_response IS '@foreignFieldName responses';
COMMENT ON CONSTRAINT json_response_frontier_id_fkey ON crawler.json_response IS '@foreignFieldName responses';
COMMENT ON CONSTRAINT competition_federation_fkey ON federated.competition IS '@foreignFieldName competitions';
COMMENT ON CONSTRAINT competition_event_id_fkey ON federated.competition IS '@foreignFieldName competitions
@backwardBehavior +list';
COMMENT ON CONSTRAINT competitor_federation_fkey ON federated.competitor IS '@foreignFieldName competitors';
COMMENT ON CONSTRAINT event_federation_fkey ON federated.event IS '@foreignFieldName events';
COMMENT ON CONSTRAINT event_organizing_club_id_fkey ON federated.event IS '@foreignFieldName events';
COMMENT ON CONSTRAINT person_federation_fkey ON federated.person IS '@foreignFieldName people';
COMMENT ON CONSTRAINT federation_club_federation_fkey ON federated.federation_club IS '@foreignFieldName clubs';
COMMENT ON CONSTRAINT person_license_federation_fkey ON federated.person_license IS '@foreignFieldName licenses';
COMMENT ON CONSTRAINT person_license_person_id_fkey ON federated.person_license IS '@foreignFieldName licenses';
COMMENT ON CONSTRAINT ranklist_federation_fkey ON federated.ranklist IS '@foreignFieldName ranklists';
COMMENT ON CONSTRAINT competitor_component_competitor_id_fkey ON federated.competitor_component IS '@foreignFieldName components';
COMMENT ON CONSTRAINT competitor_component_person_id_fkey ON federated.competitor_component IS '@foreignFieldName competitorComponents';
COMMENT ON CONSTRAINT competitor_category_progress_competitor_id_fkey ON federated.competitor_category_progress IS '@foreignFieldName categoryProgresses
@backwardBehavior +list -connection';
COMMENT ON CONSTRAINT competitor_category_progress_category_id_fkey ON federated.competitor_category_progress IS '@foreignFieldName competitorProgresses';
COMMENT ON CONSTRAINT competitor_club_affiliation_competitor_id_fkey ON federated.competitor_club_affiliation IS '@foreignFieldName clubAffiliations';
COMMENT ON CONSTRAINT competitor_club_affiliation_club_id_fkey ON federated.competitor_club_affiliation IS '@foreignFieldName competitorAffiliations';
COMMENT ON CONSTRAINT person_club_membership_person_id_fkey ON federated.person_club_membership IS '@foreignFieldName clubMemberships';
COMMENT ON CONSTRAINT person_club_membership_club_id_fkey ON federated.person_club_membership IS '@foreignFieldName personMemberships';
COMMENT ON CONSTRAINT event_official_event_id_fkey ON federated.event_official IS '@foreignFieldName officials';
COMMENT ON CONSTRAINT event_official_person_id_fkey ON federated.event_official IS '@foreignFieldName eventOfficialAssignments';
COMMENT ON CONSTRAINT competition_entry_competition_id_fkey ON federated.competition_entry IS '@foreignFieldName entries
@backwardBehavior +list';
COMMENT ON CONSTRAINT competition_entry_competitor_id_fkey ON federated.competition_entry IS '@foreignFieldName competitionEntries';
COMMENT ON CONSTRAINT competition_result_competition_id_fkey ON federated.competition_result IS '@foreignFieldName results
@backwardBehavior +list';
COMMENT ON CONSTRAINT competition_result_competitor_id_fkey ON federated.competition_result IS '@foreignFieldName competitionResults';
COMMENT ON CONSTRAINT competition_round_competition_id_fkey ON federated.competition_round IS '@foreignFieldName rounds
@backwardBehavior +list -connection';
COMMENT ON CONSTRAINT competition_official_competition_id_fkey ON federated.competition_official IS '@foreignFieldName officials';
COMMENT ON CONSTRAINT competition_official_person_id_fkey ON federated.competition_official IS '@foreignFieldName competitionOfficialAssignments';
COMMENT ON CONSTRAINT competition_round_judge_round_id_fkey ON federated.competition_round_judge IS '@foreignFieldName judges
@backwardBehavior +list -connection';
COMMENT ON CONSTRAINT competition_round_judge_person_judge_id_fkey ON federated.competition_round_judge IS '@foreignFieldName roundJudgeAssignments';
COMMENT ON CONSTRAINT competition_round_result_round_id_fkey ON federated.competition_round_result IS '@foreignFieldName results
@backwardBehavior +list -connection';
COMMENT ON CONSTRAINT competition_round_result_competitor_id_fkey ON federated.competition_round_result IS '@foreignFieldName roundResults';
COMMENT ON CONSTRAINT round_dance_round_id_dance_program_id_fkey ON federated.round_dance IS '@foreignFieldName dances
@backwardBehavior +list -connection';
COMMENT ON CONSTRAINT judge_score_round_id_fkey ON federated.judge_score IS '@foreignFieldName judgeScores
@backwardBehavior +list -connection';
COMMENT ON CONSTRAINT judge_score_round_id_dance_order_dance_code_fkey ON federated.judge_score IS '@foreignFieldName judgeScores';
COMMENT ON CONSTRAINT ranklist_entry_competitor_id_fkey ON federated.ranklist_entry IS '@foreignFieldName ranklistEntries';
COMMENT ON CONSTRAINT dance_program_dance_dance_code_fkey ON federated.dance_program_dance IS '@foreignFieldName dancePrograms';
COMMENT ON CONSTRAINT dance_program_dance_program_id_fkey ON federated.dance_program_dance IS '@foreignFieldName dances';
COMMENT ON CONSTRAINT category_base_dance_program_id_fkey ON federated.category IS '@foreignFieldName categories';
COMMENT ON CONSTRAINT competition_federation_event_id_fkey ON federated.competition IS '@behavior -single -connection -list';
COMMENT ON CONSTRAINT event_federation_organizing_club_id_fkey ON federated.event IS '@behavior -single -connection -list';
COMMENT ON CONSTRAINT round_dance_dance_program_id_dance_code_fkey ON federated.round_dance IS '@foreignFieldName roundDances';

create or replace view person_federation_link with (security_invoker = true) as
select id as person_id, 'csts'::text as federation, csts_id::bigint as external_id
from person where csts_id <> 0
union all
select id, 'wdsf'::text, wdsf_id::bigint
from person where wdsf_id <> 0;

comment on view person_federation_link is '@primaryKey person_id,federation
@foreignKey (person_id) references public.person (id)
@foreignKey (federation,external_id) references federated.person (federation,external_id)
@behavior -single -list -connection -insert -update -delete';
grant select on person_federation_link to anonymous;

comment on table person is '@omit create
@ref federationPeople via:public.person_federation_link;federated.person
@ref federationCategoryProgresses via:public.person_federation_link;federated.person;federated.competitor_component;federated.competitor;federated.competitor_category_progress
@ref federationRanklistEntries via:public.person_federation_link;federated.person;federated.competitor_component;federated.competitor;federated.ranklist_entry
@ref federationCompetitions via:public.person_federation_link;federated.person;federated.competitor_component;federated.competitor;federated.competition_participation;federated.competition';

drop function if exists csts_athlete;
drop function if exists person_csts_progress;
drop function if exists wdsf_athlete;

create or replace view couple_federation_link with (security_invoker = true) as
select c.id as couple_id, fc.id as competitor_id
from couple c
join person_federation_link ml on ml.person_id = c.man_id
join federated.person mp on (mp.federation, mp.external_id) = (ml.federation, ml.external_id)
join federated.competitor_component mc on mc.person_id = mp.id and mc.role = 'lead'
join federated.competitor fc on fc.id = mc.competitor_id and fc.competitor_type = 'couple'
join federated.competitor_component wc on wc.competitor_id = fc.id and wc.role = 'follow'
join federated.person wp on wp.id = wc.person_id
join person_federation_link wl on wl.person_id = c.woman_id
  and (wl.federation, wl.external_id) = (wp.federation, wp.external_id);

comment on view couple_federation_link is '@primaryKey couple_id,competitor_id
@foreignKey (couple_id) references public.couple (id)
@foreignKey (competitor_id) references federated.competitor (id)
@behavior -single -list -connection -insert -update -delete';
grant select on couple_federation_link to anonymous;

comment on table couple is '@simpleCollections only
@behavior -query:resource:list -query:resource:connection
@ref federationCompetitors via:public.couple_federation_link;federated.competitor
@ref federationRanklistEntries via:public.couple_federation_link;federated.competitor;federated.ranklist_entry
@ref federationCompetitions via:public.couple_federation_link;federated.competitor;federated.competition_participation;federated.competition behavior:"+connection -list"';

do $$
begin
  if not exists (select 1 from pg_type where typnamespace = 'federated'::regnamespace and typname = 'event_status') then
    create type federated.event_status as enum (
      'unknown', 'planned', 'in_progress', 'completed', 'postponed', 'cancelled'
    );
  end if;
end;
$$;
comment on type federated.event_status is '@name federation_event_status';
grant usage on type federated.event_status to anonymous;

alter table federated.event add column if not exists status federated.event_status not null default 'unknown';
alter table federated.competition add column if not exists status federated.event_status not null default 'unknown';

with cached as (
  select distinct on (e->>'id') e->>'id' as external_id,
    case (e->'eventCompetitions'->0->>'state')::text
      when 'Private' then 'planned'
      when 'Waiting' then 'planned'
      when 'Approved' then 'planned'
      when 'Public' then 'planned'
      when 'Checked' then 'planned'
      when 'Cancelled' then 'cancelled'
      when '0' then 'planned'
      when '1' then 'planned'
      when '2' then 'planned'
      when '3' then 'planned'
      when '900' then 'planned'
      when '100' then 'cancelled'
      else 'unknown'
    end::federated.event_status as status
  from crawler.frontier f
  join crawler.json_response r on r.id = f.last_successful_response_id
  join crawler.json_response_cache cache using (content_hash)
  cross join lateral jsonb_array_elements(cache.content->'collection') e
  where f.federation = 'csts' and f.kind = 'eventIndex'
  order by e->>'id', r.last_observed_at desc, r.id desc
)
update federated.event e set status = cached.status
from cached
where e.federation = 'csts' and e.external_id = cached.external_id
  and e.status = 'unknown' and e.status is distinct from cached.status;

with cached as (
  select f.key, case cache.content->>'status'
    when 'Canceled' then 'cancelled'
    when 'Closed' then 'completed'
    when 'RegistrationClosed' then 'planned'
    when 'Approved' then 'planned'
    when 'PreRegistration' then 'planned'
    when 'Registering' then 'planned'
    when 'InProgress' then 'in_progress'
    when 'Processing' then 'in_progress'
    when 'PostponedTBC' then 'postponed'
    when 'Postponed' then 'postponed'
    else 'unknown'
  end::federated.event_status as status
  from crawler.frontier f
  join crawler.json_response r on r.id = f.last_successful_response_id
  join crawler.json_response_cache cache using (content_hash)
  where f.federation = 'wdsf' and f.kind = 'competition'
)
update federated.competition c set status = cached.status
from cached
where c.federation = 'wdsf' and c.external_id = cached.key
  and c.status = 'unknown' and c.status is distinct from cached.status;

update federated.competition set status = case
  when completed_at is not null then 'completed'::federated.event_status
  else 'planned'::federated.event_status
end
where federation = 'csts' and status = 'unknown';

with statuses as (
  select c.event_id, case
    when bool_and(c.status = 'cancelled') then 'cancelled'
    when bool_and(c.status in ('completed', 'cancelled')) then 'completed'
    when bool_or(c.status = 'in_progress') then 'in_progress'
    when bool_or(c.status = 'planned') then 'planned'
    when bool_or(c.status = 'postponed') then 'postponed'
    else 'unknown'
  end::federated.event_status as status
  from federated.competition c
  where c.federation = 'wdsf'
  group by c.event_id
)
update federated.event e set status = statuses.status
from statuses
where e.federation = 'wdsf' and e.id = statuses.event_id
  and e.status is distinct from statuses.status;


ALTER TABLE federated.competition_official ADD COLUMN IF NOT EXISTS judge_label text;

--! Included functions/competition_reports.sql
create or replace function competition_brief(
  p_since date default (date_trunc('week', now())::date + 4),
  p_until date default (date_trunc('week', now())::date + 7),
  p_cohort_id bigint default null,
  p_person_ids bigint[] default null
) returns setof competition_participation_record
  language sql stable
as $$
  with scoped_people as (
    select p.id, p.name
    from person p
    where (p.id = any(p_person_ids)
       or (p_person_ids is null and exists (
         select 1 from current_tenant_membership where person_id = p.id
       )))
      and (p_cohort_id is null
       or exists (select 1 from current_cohort_membership cm where cm.person_id = p.id and cm.cohort_id = p_cohort_id))
  ),
  federated_people as (
    select
      sp.id as person_id,
      sp.name as person_name,
      fp.id as federated_person_id,
      fp.federation
    from scoped_people sp
    join person_federation_link link on link.person_id = sp.id
    join federated.person fp
      on (fp.federation, fp.external_id) = (link.federation, link.external_id)
  )
  select
    fp.person_id,
    fp.person_name,
    fp.federation,
    fp.federated_person_id,
    c.id as competitor_id,
    c.name as competitor_name,
    c.competitor_type,
    e.id as event_id,
    e.name as event_name,
    coalesce(e.location, e.city) as event_location,
    comp.id as competition_id,
    comp.start_date as competition_date,
    comp.check_in_end,
    cat as category,
    dances.dances,
    comp.participants_total as participants,
    null::integer as ranking,
    null::integer as ranking_to,
    null::numeric(10,3) as point_gain,
    null::boolean as is_final,
    false as has_result,
    comp.competition_type,
    e.external_id as event_external_id,
    comp.external_id as competition_external_id
  from federated_people fp
  join federated.competitor_component cc on cc.person_id = fp.federated_person_id
  join federated.competitor c on c.id = cc.competitor_id
  join federated.competition_entry ce on ce.competitor_id = c.id and not ce.cancelled
  join federated.competition comp on comp.id = ce.competition_id
  join federated.event e on e.id = comp.event_id
  join federated.category cat on cat.id = comp.category_id
  left join lateral (
    select coalesce(array_agg(d.name order by dpd.dance_order), '{}'::text[]) as dances
    from federated.dance_program_dance dpd
    join federated.dance d on d.code = dpd.dance_code
    where dpd.program_id = cat.base_dance_program_id
  ) dances on true
  where comp.start_date >= p_since
    and comp.start_date < p_until
  order by
    comp.start_date,
    comp.check_in_end nulls last,
    fp.person_name,
    cat.discipline,
    cat.class,
    c.name;
$$;

create or replace function competition_report(
  p_since date default (date_trunc('week', now())::date - 2),
  p_until date default date_trunc('week', now())::date,
  p_cohort_id bigint default null,
  p_person_ids bigint[] default null
) returns setof competition_participation_record
  language sql stable
as $$
  with scoped_people as (
    select p.id, p.name
    from person p
    where (p.id = any(p_person_ids)
       or (p_person_ids is null and exists (
         select 1 from current_tenant_membership where person_id = p.id
       )))
      and (p_cohort_id is null
       or exists (select 1 from current_cohort_membership cm where cm.person_id = p.id and cm.cohort_id = p_cohort_id))
  ),
  federated_people as (
    select
      sp.id as person_id,
      sp.name as person_name,
      fp.id as federated_person_id,
      fp.federation
    from scoped_people sp
    join person_federation_link link on link.person_id = sp.id
    join federated.person fp
      on (fp.federation, fp.external_id) = (link.federation, link.external_id)
  )
  select
    fp.person_id,
    fp.person_name,
    fp.federation,
    fp.federated_person_id,
    c.id as competitor_id,
    c.name as competitor_name,
    c.competitor_type,
    e.id as event_id,
    e.name as event_name,
    coalesce(e.location, e.city) as event_location,
    comp.id as competition_id,
    comp.start_date as competition_date,
    comp.check_in_end,
    cat as category,
    coalesce(dances.dances, '{}'::text[]) as dances,
    comp.participants_total as participants,
    cr.ranking,
    cr.ranking_to,
    cr.point_gain,
    cr.is_final,
    true as has_result,
    comp.competition_type,
    e.external_id as event_external_id,
    comp.external_id as competition_external_id
  from federated_people fp
  join federated.competitor_component cc on cc.person_id = fp.federated_person_id
  join federated.competitor c on c.id = cc.competitor_id
  join federated.competition_result cr on cr.competitor_id = c.id
  join federated.competition comp on comp.id = cr.competition_id
  join federated.event e on e.id = comp.event_id
  join federated.category cat on cat.id = comp.category_id
  left join lateral (
    select array_agg(d.name order by rd.dance_order) as dances
    from (
      select r.id
      from federated.competition_round r
      where r.competition_id = comp.id
      order by r.round_index desc, (r.round_key = 'F') desc, r.id desc
      limit 1
    ) r
    join federated.round_dance rd on rd.round_id = r.id
    join federated.dance d on d.code = rd.dance_code
  ) dances on true
  where comp.start_date >= p_since
    and comp.start_date < p_until
  order by
    comp.start_date,
    fp.person_name,
    cat.discipline,
    cat.class,
    cr.ranking,
    c.name;
$$;

comment on function competition_brief is '@omit';
grant all on function competition_brief to anonymous;

comment on function competition_report is '@omit';
grant all on function competition_report to anonymous;
--! EndIncluded functions/competition_reports.sql
--! Included functions/activity_timeline.sql
create or replace view activity_timeline_item as
select
  null::text as id,
  null::activity_timeline_kind as kind,
  null::timestamptz as sort_at,
  null::date as activity_date,
  null::bigint as person_id,
  null::text as person_name,
  null::bigint as event_attendance_id,
  null::bigint as event_instance_id,
  null::text as federation,
  null::text as federated_person_id,
  null::text as competitor_id,
  null::text as competitor_name,
  null::federated.competitor_type as competitor_type,
  null::bigint as competition_event_id,
  null::text as competition_event_name,
  null::text as competition_event_location,
  null::bigint as competition_id,
  null::date as competition_date,
  null::time as check_in_end,
  null::federated.category as category,
  null::text[] as dances,
  null::integer as participants,
  null::integer as ranking,
  null::integer as ranking_to,
  null::numeric(10, 3) as point_gain,
  null::boolean as is_final,
  null::federated.competition_type as competition_type,
  null::text as competition_event_external_id,
  null::text as competition_external_id
where false;

comment on view activity_timeline_item is $$
@primaryKey id
@interface mode:single type:kind
@type EVENT_ATTENDANCE name:ActivityEventAttendance attributes:event_attendance_id,event_instance_id
@type COMPETITION_BRIEF name:ActivityCompetitionBrief attributes:federation,federated_person_id,competitor_id,competitor_name,competitor_type,competition_event_id,competition_event_name,competition_event_location,competition_id,competition_date,check_in_end,category,dances,participants,competition_type,competition_event_external_id,competition_external_id
@type COMPETITION_RESULT name:ActivityCompetitionResult attributes:federation,federated_person_id,competitor_id,competitor_name,competitor_type,competition_event_id,competition_event_name,competition_event_location,competition_id,competition_date,category,dances,participants,ranking,ranking_to,point_gain,is_final,competition_type,competition_event_external_id,competition_external_id
@type JUDGING name:ActivityJudging attributes:federation,federated_person_id,competition_event_id,competition_event_name,competition_event_location,competition_id,competition_date,category,competition_type,competition_event_external_id,competition_external_id
@type BIRTHDAY name:ActivityBirthday
@foreignKey (person_id) references person (id)|@fieldName person|@behavior -manyRelation:resource:list -manyRelation:resource:connection
@foreignKey (event_attendance_id) references event_instance_registration (id)|@fieldName eventAttendance|@behavior -manyRelation:resource:list -manyRelation:resource:connection
@foreignKey (event_instance_id) references event_instance (id)|@fieldName eventInstance|@behavior -manyRelation:resource:list -manyRelation:resource:connection
@behavior -query:resource:list -query:resource:connection -query:resource:single
$$;

grant select on activity_timeline_item to anonymous;

create or replace function activity_timeline(p_since timestamp with time zone, p_until timestamp with time zone, p_person_ids bigint[] DEFAULT NULL::bigint[], p_cohort_id bigint DEFAULT NULL::bigint, p_kinds activity_timeline_kind[] DEFAULT NULL::activity_timeline_kind[], p_event_types event_type[] DEFAULT NULL::event_type[])
 returns setof activity_timeline_item
 language plpgsql
 stable
as $function$
declare
  include_event_attendance boolean;
  include_competition_brief boolean;
  include_competition_result boolean;
  include_judging boolean;
  include_birthday boolean;
begin
  if p_since is null or p_until is null or p_until <= p_since then
    return;
  end if;

  if cardinality(p_kinds) = 0 then
    return;
  end if;

  include_event_attendance =
    (p_kinds is null or 'EVENT_ATTENDANCE'::activity_timeline_kind = any(p_kinds))
    and (p_event_types is null or cardinality(p_event_types) > 0);
  include_competition_brief =
    p_kinds is null or 'COMPETITION_BRIEF'::activity_timeline_kind = any(p_kinds);
  include_competition_result =
    p_kinds is null or 'COMPETITION_RESULT'::activity_timeline_kind = any(p_kinds);
  include_judging =
    p_kinds is null or 'JUDGING'::activity_timeline_kind = any(p_kinds);
  include_birthday =
    p_kinds is null or 'BIRTHDAY'::activity_timeline_kind = any(p_kinds);

  if include_event_attendance then
    return query
      with scoped_people as (
        select distinct p.id, p.name
        from person p
        where (exists (select 1 from current_tenant_membership where person_id = p.id)
           or exists (select 1 from current_tenant_trainer where person_id = p.id)
           or exists (select 1 from current_tenant_administrator where person_id = p.id))
          and (p_person_ids is null or p.id = any(p_person_ids))
          and (p_cohort_id is null or exists (select 1 from current_cohort_membership cm where cm.person_id = p.id and cm.cohort_id = p_cohort_id))
      )
      select
        ('event_attendance:' || ea.id)::text as id,
        'EVENT_ATTENDANCE'::activity_timeline_kind as kind,
        ei.since as sort_at,
        ei.since::date as activity_date,
        ea.person_id,
        sp.name as person_name,
        ea.id as event_attendance_id,
        ei.id as event_instance_id,
        null::text as federation,
        null::text as federated_person_id,
        null::text as competitor_id,
        null::text as competitor_name,
        null::federated.competitor_type as competitor_type,
        null::bigint as competition_event_id,
        null::text as competition_event_name,
        null::text as competition_event_location,
        null::bigint as competition_id,
        null::date as competition_date,
        null::time as check_in_end,
        null::federated.category as category,
        null::text[] as dances,
        null::integer as participants,
        null::integer as ranking,
        null::integer as ranking_to,
        null::numeric(10, 3) as point_gain,
        null::boolean as is_final,
        null::federated.competition_type as competition_type,
        null::text as competition_event_external_id,
        null::text as competition_external_id
      from event_instance_registration ea
      join event_instance ei on ei.id = ea.instance_id
      join scoped_people sp on sp.id = ea.person_id
      where ea.registration_status = 'active'
        and ei.since >= p_since
        and ei.since < p_until
        and (p_event_types is null or ei.type = any(p_event_types));
  end if;

  if include_birthday then
    return query
      with scoped_people as (
        select distinct p.id, p.name, p.birth_date
        from person p
        where (exists (select 1 from current_tenant_membership where person_id = p.id)
           or exists (select 1 from current_tenant_trainer where person_id = p.id)
           or exists (select 1 from current_tenant_administrator where person_id = p.id))
          and (p_person_ids is null or p.id = any(p_person_ids))
          and (p_cohort_id is null or exists (select 1 from current_cohort_membership cm where cm.person_id = p.id and cm.cohort_id = p_cohort_id))
          and p.birth_date is not null
      ),
      birthdays as (
        select
          sp.id as person_id,
          sp.name as person_name,
          make_date(
            years.year,
            extract(month from sp.birth_date)::int,
            least(
              extract(day from sp.birth_date)::int,
              extract(day from (
                make_date(years.year, extract(month from sp.birth_date)::int, 1)
                + interval '1 month - 1 day'
              ))::int
            )
          ) as birthday_date
        from scoped_people sp
        cross join generate_series(extract(year from p_since)::int, extract(year from p_until)::int) as years(year)
      )
      select
        ('birthday:' || b.person_id || ':' || b.birthday_date)::text as id,
        'BIRTHDAY'::activity_timeline_kind as kind,
        ((b.birthday_date + time '12:00')::timestamp)::timestamptz as sort_at,
        b.birthday_date as activity_date,
        b.person_id,
        b.person_name,
        null::bigint as event_attendance_id,
        null::bigint as event_instance_id,
        null::text as federation,
        null::text as federated_person_id,
        null::text as competitor_id,
        null::text as competitor_name,
        null::federated.competitor_type as competitor_type,
        null::bigint as competition_event_id,
        null::text as competition_event_name,
        null::text as competition_event_location,
        null::bigint as competition_id,
        null::date as competition_date,
        null::time as check_in_end,
        null::federated.category as category,
        null::text[] as dances,
        null::integer as participants,
        null::integer as ranking,
        null::integer as ranking_to,
        null::numeric(10, 3) as point_gain,
        null::boolean as is_final,
        null::federated.competition_type as competition_type,
        null::text as competition_event_external_id,
        null::text as competition_external_id
      from birthdays b
      join person p on p.id = b.person_id
      where b.birthday_date >= p.birth_date
        and ((b.birthday_date + time '12:00')::timestamp)::timestamptz >= p_since
        and ((b.birthday_date + time '12:00')::timestamp)::timestamptz < p_until;
  end if;

  if include_competition_result then
    return query
      select
        (
          'competition_result:' ||
          coalesce(cr.competition_id::text, '') || ':' ||
          coalesce(cr.competitor_id, '') || ':' ||
          coalesce(cr.person_id::text, '') || ':' ||
          coalesce((cr.category).id::text, '')
        )::text as id,
        'COMPETITION_RESULT'::activity_timeline_kind as kind,
        ((cr.competition_date + time '12:00')::timestamp)::timestamptz as sort_at,
        cr.competition_date as activity_date,
        cr.person_id,
        cr.person_name,
        null::bigint as event_attendance_id,
        null::bigint as event_instance_id,
        cr.federation,
        cr.federated_person_id,
        cr.competitor_id,
        cr.competitor_name,
        cr.competitor_type,
        cr.event_id as competition_event_id,
        cr.event_name as competition_event_name,
        cr.event_location as competition_event_location,
        cr.competition_id,
        cr.competition_date,
        null::time as check_in_end,
        cr.category,
        cr.dances,
        cr.participants,
        cr.ranking,
        cr.ranking_to,
        cr.point_gain,
        cr.is_final,
        cr.competition_type,
        cr.event_external_id as competition_event_external_id,
        cr.competition_external_id
      from (
        select * from competition_report(p_since::date, p_until::date, p_cohort_id, p_person_ids)
      ) as cr
      where cr.competition_date is not null
        and ((cr.competition_date + time '12:00')::timestamp)::timestamptz >= p_since
        and ((cr.competition_date + time '12:00')::timestamp)::timestamptz < p_until;
  end if;

  if include_competition_brief then
    return query
      with reports as (
        select
          cr.person_id,
          cr.competition_id,
          cr.competitor_id,
          (cr.category).id as category_id
        from (
          select * from competition_report(p_since::date, p_until::date, p_cohort_id, p_person_ids)
        ) as cr
        where include_competition_result
      )
      select
        (
          'competition_brief:' ||
          coalesce(cb.competition_id::text, '') || ':' ||
          coalesce(cb.competitor_id, '') || ':' ||
          coalesce(cb.person_id::text, '') || ':' ||
          coalesce((cb.category).id::text, '')
        )::text as id,
        'COMPETITION_BRIEF'::activity_timeline_kind as kind,
        ((cb.competition_date + coalesce(cb.check_in_end, time '12:00'))::timestamp)::timestamptz as sort_at,
        cb.competition_date as activity_date,
        cb.person_id,
        cb.person_name,
        null::bigint as event_attendance_id,
        null::bigint as event_instance_id,
        cb.federation,
        cb.federated_person_id,
        cb.competitor_id,
        cb.competitor_name,
        cb.competitor_type,
        cb.event_id as competition_event_id,
        cb.event_name as competition_event_name,
        cb.event_location as competition_event_location,
        cb.competition_id,
        cb.competition_date,
        cb.check_in_end,
        cb.category,
        cb.dances,
        cb.participants,
        null::integer as ranking,
        null::integer as ranking_to,
        null::numeric(10, 3) as point_gain,
        null::boolean as is_final,
        cb.competition_type,
        cb.event_external_id as competition_event_external_id,
        cb.competition_external_id
      from (
        select * from competition_brief(p_since::date, p_until::date, p_cohort_id, p_person_ids)
      ) as cb
      where cb.competition_date is not null
        and ((cb.competition_date + coalesce(cb.check_in_end, time '12:00'))::timestamp)::timestamptz >= p_since
        and ((cb.competition_date + coalesce(cb.check_in_end, time '12:00'))::timestamp)::timestamptz < p_until
        and not exists (
          select 1
          from reports r
          where r.person_id is not distinct from cb.person_id
            and r.competition_id is not distinct from cb.competition_id
            and r.competitor_id is not distinct from cb.competitor_id
            and r.category_id is not distinct from (cb.category).id
        );
  end if;

  if include_judging then
    return query
      with scoped_people as (
        select p.id, p.name
        from person p
        where (p.id = any(p_person_ids)
           or (p_person_ids is null and (
             exists (select 1 from current_tenant_membership where person_id = p.id)
             or exists (select 1 from current_tenant_trainer where person_id = p.id)
             or exists (select 1 from current_tenant_administrator where person_id = p.id)
           )))
          and (p_cohort_id is null or exists (select 1 from current_cohort_membership cm where cm.person_id = p.id and cm.cohort_id = p_cohort_id))
      ),
      federated_people as (
        select
          sp.id as person_id,
          sp.name as person_name,
          fp.id as federated_person_id,
          fp.federation
        from scoped_people sp
        join person_federation_link link on link.person_id = sp.id
        join federated.person fp
          on (fp.federation, fp.external_id) = (link.federation, link.external_id)
      )
      select distinct
        ('judging:event:' || e.id || ':' || fp.person_id)::text as id,
        'JUDGING'::activity_timeline_kind as kind,
        ((e.start_date + time '12:00')::timestamp)::timestamptz as sort_at,
        e.start_date as activity_date,
        fp.person_id,
        fp.person_name,
        null::bigint as event_attendance_id,
        null::bigint as event_instance_id,
        e.federation,
        fp.federated_person_id,
        null::text as competitor_id,
        null::text as competitor_name,
        null::federated.competitor_type as competitor_type,
        e.id as competition_event_id,
        e.name as competition_event_name,
        coalesce(e.location, e.city) as competition_event_location,
        null::bigint as competition_id,
        e.start_date as competition_date,
        null::time as check_in_end,
        null::federated.category as category,
        null::text[] as dances,
        null::integer as participants,
        null::integer as ranking,
        null::integer as ranking_to,
        null::numeric(10, 3) as point_gain,
        null::boolean as is_final,
        null::federated.competition_type as competition_type,
        e.external_id as competition_event_external_id,
        null::text as competition_external_id
      from federated.event_official eo
      join federated.event e on e.id = eo.event_id
      join federated_people fp on fp.federated_person_id = eo.person_id
      where eo.role = 'adjudicator'
        and e.start_date >= current_date
        and ((e.start_date + time '12:00')::timestamp)::timestamptz >= p_since
        and ((e.start_date + time '12:00')::timestamp)::timestamptz < p_until
      union all
      select
        ('judging:competition:' || c.id || ':' || fp.person_id)::text as id,
        'JUDGING'::activity_timeline_kind as kind,
        ((c.start_date + time '12:00')::timestamp)::timestamptz as sort_at,
        c.start_date as activity_date,
        fp.person_id,
        fp.person_name,
        null::bigint as event_attendance_id,
        null::bigint as event_instance_id,
        c.federation,
        fp.federated_person_id,
        null::text as competitor_id,
        null::text as competitor_name,
        null::federated.competitor_type as competitor_type,
        e.id as competition_event_id,
        e.name as competition_event_name,
        coalesce(e.location, e.city) as competition_event_location,
        c.id as competition_id,
        c.start_date as competition_date,
        null::time as check_in_end,
        cat as category,
        null::text[] as dances,
        null::integer as participants,
        null::integer as ranking,
        null::integer as ranking_to,
        null::numeric(10, 3) as point_gain,
        null::boolean as is_final,
        c.competition_type,
        e.external_id as competition_event_external_id,
        c.external_id as competition_external_id
      from federated.competition_official co
      join federated.competition c on c.id = co.competition_id
      join federated.event e on e.id = c.event_id
      join federated.category cat on cat.id = c.category_id
      join federated_people fp on fp.federated_person_id = co.person_id
      where co.role = 'adjudicator'
        and c.start_date < current_date
        and ((c.start_date + time '12:00')::timestamp)::timestamptz >= p_since
        and ((c.start_date + time '12:00')::timestamp)::timestamptz < p_until;
  end if;
end;
$function$;

comment on function activity_timeline is '@behavior +queryField:resource:list -queryField:resource:connection';
grant all on function activity_timeline to anonymous;
--! EndIncluded functions/activity_timeline.sql

--! Included views/crawler_frontier_summary.sql
create or replace view crawler.frontier_summary as
select
  federation,
  kind,
  count(*)::int as total,
  count(*) filter (where fetch_status = 'pending')::int as fetch_pending,
  count(*) filter (where fetch_status = 'transient')::int as fetch_transient,
  count(*) filter (where fetch_status = 'error')::int as fetch_error,
  count(*) filter (where process_status = 'ok')::int as done,
  count(*) filter (where process_status = 'error')::int as process_error,
  count(*) filter (where process_status = 'pending' and fetch_status = 'ok')::int as process_ready,
  max(last_fetched_at) as latest,
  count(*) filter (where next_fetch_at <= now() and fetch_status in ('pending', 'transient'))::int as fetch_due,
  count(*) filter (where next_fetch_at <= now() and fetch_status = 'ok' and process_status = 'ok')::int as refetch_due
from crawler.frontier
group by federation, kind;

comment on view crawler.frontier_summary is '@name crawler_target_summary
@behavior +list -connection -insert -update -delete';
revoke all on crawler.frontier_summary from public;
grant select on crawler.frontier_summary to system_admin;
--! EndIncluded views/crawler_frontier_summary.sql
--! Included views/crawler_frontier_failure_group.sql
create or replace view crawler.frontier_failure_group as
select
  federation,
  kind,
  failure,
  http_status,
  left(regexp_replace(coalesce(nullif(error_text, ''), '-'), '\s+', ' ', 'g'), 180) as error_fingerprint,
  count(*)::int as count,
  max(failed_at) as failed_at
from crawler.frontier_failure
group by federation, kind, failure, http_status, error_fingerprint;

comment on view crawler.frontier_failure_group is '@name crawler_target_failure_group
@behavior +list -connection -insert -update -delete';
revoke all on crawler.frontier_failure_group from public;
grant select on crawler.frontier_failure_group to system_admin;
--! EndIncluded views/crawler_frontier_failure_group.sql

create table if not exists federated.age_group (
  code text primary key,
  name text not null,
  aliases text[] not null default '{}',
  allowed_to_dance_in text[] not null default '{}',
  age int4range check (not isempty(age)),
  person_age int4range check (not isempty(person_age)),
  is_person boolean not null default false,
  is_couple boolean not null default false,
  is_team boolean not null default false,
  is_competition boolean not null default false,
  divisions text[] not null default '{}'
);
comment on table federated.age_group is '@name age_group
@behavior -insert -update -delete +list -connection';
comment on column federated.age_group.aliases is '@behavior -filterBy -orderBy';
comment on column federated.age_group.allowed_to_dance_in is '@behavior -filterBy -orderBy';
comment on column federated.age_group.age is '@behavior -filterBy -orderBy';
comment on column federated.age_group.person_age is '@behavior -filterBy -orderBy';
comment on column federated.age_group.divisions is '@behavior -filterBy -orderBy';
grant select on federated.age_group to anonymous;

create table if not exists federated.couple_age_rule (
  age_group_code text not null references federated.age_group(code),
  division text not null default 'General',
  older_age int4range not null check (not isempty(older_age)),
  younger_age int4range not null check (not isempty(younger_age)),
  primary key (age_group_code, division)
);
comment on table federated.couple_age_rule is '@name federation_couple_age_rule
@behavior -insert -update -delete +list -connection -query:resource:single -query:resource:list -query:resource:connection';
comment on column federated.couple_age_rule.older_age is '@behavior -filterBy -orderBy';
comment on column federated.couple_age_rule.younger_age is '@behavior -filterBy -orderBy';
comment on constraint couple_age_rule_age_group_code_fkey on federated.couple_age_rule is '@fieldName ageGroupDetails
@foreignFieldName coupleRules
@foreignSimpleFieldName coupleRules
@backwardBehavior +list -connection';
grant select on federated.couple_age_rule to anonymous;

-- The non-overlapping person display ladder is separate from classification/entry rules.
insert into federated.age_group as t (code, name, person_age, is_person)
values
  ('Adult', 'Adult', '[21,30)', true),
  ('Adult_O', 'Adult O', null, false),
  ('Adult_T', 'Adult T', null, false),
  ('Junior', 'Junior', null, false),
  ('Junior_I', 'Junior I', '[12,14)', true),
  ('Junior_II', 'Junior II', '[14,16)', true),
  ('Junior_T', 'Junior T', null, false),
  ('Juvenile', 'Juvenile', null, false),
  ('Juvenile_I', 'Juvenile I', '[8,10)', true),
  ('Juvenile_I_C', 'Juvenile I C', null, false),
  ('Juvenile_II', 'Juvenile II', '[10,12)', true),
  ('Juvenile_II_C', 'Juvenile II C', null, false),
  ('Juvenile_T', 'Juvenile T', null, false),
  ('Kid_I', 'Kid I', null, false),
  ('Kid_II', 'Kid II', null, false),
  ('Master_Class', 'Master Class', null, false),
  ('Master_Class_I', 'Master Class I', null, false),
  ('Master_Class_II', 'Master Class II', null, false),
  ('Master_Class_III', 'Master Class III', null, false),
  ('Mix', 'Mix', null, false),
  ('Over_35', 'Over 35', null, false),
  ('Rising_Star', 'Rising Star', null, false),
  ('Senior', 'Senior', null, false),
  ('Senior_I', 'Senior I', null, false),
  ('Senior_Ia', 'Senior Ia', '[30,35)', true),
  ('Senior_Ib', 'Senior Ib', '[35,40)', true),
  ('Senior_II', 'Senior II', null, false),
  ('Senior_IIa', 'Senior IIa', '[40,45)', true),
  ('Senior_IIb', 'Senior IIb', '[45,50)', true),
  ('Senior_III', 'Senior III', null, false),
  ('Senior_IIIa', 'Senior IIIa', '[50,55)', true),
  ('Senior_IIIb', 'Senior IIIb', '[55,60)', true),
  ('Senior_IV', 'Senior IV', null, false),
  ('Senior_IVa', 'Senior IVa', '[60,65)', true),
  ('Senior_IVb', 'Senior IVb', '[65,70)', true),
  ('Senior_O', 'Senior O', null, false),
  ('Senior_T', 'Senior T', null, false),
  ('Senior_V', 'Senior V', '[70,)', true),
  ('Under_21', 'Under 21', '[19,21)', true),
  ('Youth', 'Youth', '[16,19)', true),
  ('Youth_X', 'Youth X', null, false),
  ('Under_8', 'Under 8', '[0,8)', true)
on conflict (code) do update set person_age = excluded.person_age
where t.person_age is distinct from excluded.person_age;

update federated.age_group g set aliases = a.aliases::text[]
from (values
  ('Under_8', '{u8}'), ('Juvenile_I', '{u10}'), ('Juvenile_II', '{u12}'),
  ('Junior_I', '{u14}'), ('Junior_II', '{u16}'), ('Youth', '{u19,under19}'),
  ('Under_21', '{u21}')
) a(code, aliases)
where g.code = a.code and g.aliases is distinct from a.aliases::text[];

--! Included functions/resolve_age_group.sql
create or replace function federated.resolve_age_group(source_name text) returns text
language sql stable strict as $$
  select code from federated.age_group
  where regexp_replace(lower(source_name), '[^a-z0-9]', '', 'g')
    = any(array_prepend(regexp_replace(lower(code), '[^a-z0-9]', '', 'g'), aliases));
$$;
comment on function federated.resolve_age_group(text) is '@behavior -*';
revoke all on function federated.resolve_age_group(text) from public;
grant execute on function federated.resolve_age_group(text) to anonymous;
--! EndIncluded functions/resolve_age_group.sql

insert into federated.couple_age_rule as t (age_group_code, older_age, younger_age)
values
  ('Under_8', '(,8)', '(,8)'),
  ('Juvenile_I', '(,10)', '(,10)'),
  ('Juvenile_II', '[10,12)', '(,12)'),
  ('Junior_I', '[12,14)', '(,14)'),
  ('Junior_II', '[14,16)', '(,16)'),
  ('Youth', '[16,19)', '(,19)'),
  ('Under_21', '[16,21)', '(,21)'),
  ('Adult', '[19,)', '(,35)'),
  ('Senior_I', '[35,)', '[30,)'),
  ('Senior_II', '[45,)', '[40,)'),
  ('Senior_III', '[55,)', '[50,)'),
  ('Senior_IV', '[65,)', '[60,)'),
  ('Senior_V', '[70,)', '[70,)')
on conflict (age_group_code, division) do update set
  older_age = excluded.older_age, younger_age = excluded.younger_age
where (t.older_age, t.younger_age) is distinct from (excluded.older_age, excluded.younger_age);

update federated.age_group g set is_couple = true, is_competition = true
where exists (select 1 from federated.couple_age_rule r where r.age_group_code = g.code)
  and (not g.is_couple or not g.is_competition);

-- Local additions survive every WDSF refresh.
update federated.age_group g set allowed_to_dance_in = array(
  select distinct unnest(g.allowed_to_dance_in || a.allowed::text[]) order by 1
)
from (values
  ('Under_8', '{Under_8,Juvenile_I,Juvenile_II}'),
  ('Under_21', '{Under_21,Adult}')
) a(code, allowed)
where g.code = a.code and not g.allowed_to_dance_in @> a.allowed::text[];

alter table federated.category add column if not exists age_group_code text references federated.age_group(code);
alter table federated.person add column if not exists age_group_code text references federated.age_group(code);
alter table federated.competitor add column if not exists age_group_code text references federated.age_group(code);
create index if not exists category_age_group_code_idx on federated.category(age_group_code);
create index if not exists person_age_group_code_idx on federated.person(age_group_code);
create index if not exists competitor_age_group_code_idx on federated.competitor(age_group_code);

--! Included functions/tg_resolve_age_group.sql
create or replace function app_private.tg_resolve_age_group() returns trigger
language plpgsql as $$
begin
  new.age_group_code := federated.resolve_age_group(new.age_group);
  return new;
end;
$$;
comment on function app_private.tg_resolve_age_group() is '@behavior -*';
revoke all on function app_private.tg_resolve_age_group() from public;
--! EndIncluded functions/tg_resolve_age_group.sql

create or replace trigger _resolve_age_group
  before insert or update of age_group on federated.category
  for each row execute function app_private.tg_resolve_age_group();
create or replace trigger _resolve_age_group
  before insert or update of age_group on federated.person
  for each row execute function app_private.tg_resolve_age_group();
create or replace trigger _resolve_age_group
  before insert or update of age_group on federated.competitor
  for each row execute function app_private.tg_resolve_age_group();

comment on column federated.category.age_group is '@behavior -*';
comment on column federated.person.age_group is '@behavior -*';
comment on column federated.competitor.age_group is '@behavior -*';
comment on column federated.category.age_group_code is '@name ageGroup
@behavior -insert -update';
comment on column federated.person.age_group_code is '@name ageGroup
@behavior -insert -update';
comment on column federated.competitor.age_group_code is '@name ageGroup
@behavior -insert -update';
comment on constraint category_age_group_code_fkey on federated.category is '@fieldName ageGroupDetails
@foreignFieldName categories
@foreignSimpleFieldName categories
@backwardBehavior +list -connection';
comment on constraint person_age_group_code_fkey on federated.person is '@fieldName ageGroupDetails
@backwardBehavior -single -list -connection';
comment on constraint competitor_age_group_code_fkey on federated.competitor is '@fieldName ageGroupDetails
@backwardBehavior -single -list -connection';

update federated.category set age_group_code = federated.resolve_age_group(age_group)
where age_group_code is distinct from federated.resolve_age_group(age_group);
update federated.person set age_group_code = federated.resolve_age_group(age_group)
where age_group_code is distinct from federated.resolve_age_group(age_group);
update federated.competitor set age_group_code = federated.resolve_age_group(age_group)
where age_group_code is distinct from federated.resolve_age_group(age_group);

--! Included policies/crawler.sql
alter table crawler.frontier enable row level security;
select app_private.drop_policies('crawler.frontier');
create policy system_admin_read on crawler.frontier for select to system_admin using (true);

alter table crawler.json_response enable row level security;
select app_private.drop_policies('crawler.json_response');
create policy system_admin_read on crawler.json_response for select to system_admin using (true);

alter table crawler.json_response_cache enable row level security;
select app_private.drop_policies('crawler.json_response_cache');
create policy system_admin_read on crawler.json_response_cache for select to system_admin using (true);

alter table crawler.incremental_ranges enable row level security;
select app_private.drop_policies('crawler.incremental_ranges');
create policy system_admin_read on crawler.incremental_ranges for select to system_admin using (true);

alter table crawler.rate_limit_rule enable row level security;
select app_private.drop_policies('crawler.rate_limit_rule');
create policy system_admin_read on crawler.rate_limit_rule for select to system_admin using (true);

alter view crawler.frontier_failure set (security_invoker = true);
alter view crawler.frontier_summary set (security_invoker = true);
alter view crawler.frontier_failure_group set (security_invoker = true);
--! EndIncluded policies/crawler.sql
alter table federated.competitor add column if not exists search_name text
  generated always as (app_private.normalize_name(name)) stored;
comment on column federated.competitor.search_name is '@behavior -*';
create index if not exists competitor_search_name_trgm_idx
  on federated.competitor using gin (search_name public.gin_trgm_ops);

-- Exact external-ID searches share an OR with the name search.
create index if not exists competitor_external_id_text_idx on federated.competitor ((external_id::text));
create index if not exists person_external_id_text_idx on federated.person ((external_id::text));

--! Included functions/federation_competitor_facets.sql
create or replace function federated.federation_competitor_facets(
  federation federated.federation,
  search text default null,
  competitor_types federated.competitor_type[] default null,
  age_groups text[] default null,
  classes text[] default null,
  disciplines text[] default null,
  competed_since date default null
) returns table(facet text, value text, count integer)
language sql stable as $$
  with candidates as materialized (
    select c.id, c.competitor_type::text as competitor_type,
      cat.age_group_code, cat.class, cat.discipline,
      coalesce(cardinality(competitor_types), 0) = 0 or c.competitor_type = any(competitor_types) as type_matches,
      coalesce(cardinality(age_groups), 0) = 0 or cat.age_group_code = any(
        array(select federated.resolve_age_group(value) from unnest(age_groups) value)
      ) as age_matches,
      coalesce(cardinality(classes), 0) = 0 or cat.class = any(classes) as class_matches,
      coalesce(cardinality(disciplines), 0) = 0 or cat.discipline = any(disciplines) as discipline_matches
    from federated.competitor c
    left join federated.competitor_category_progress p on p.competitor_id = c.id
    left join federated.category cat on cat.id = p.category_id
    where c.federation = federation.code
      and (nullif(trim(search), '') is null or c.external_id::text = trim(search) or c.id = trim(search)
        or c.search_name like '%' || replace(replace(replace(
          app_private.normalize_name(trim(search)), '!', '!!'), '%', '!%'), '_', '!_') || '%' escape '!')
      and (competed_since is null or exists (
        select 1 from federated.competition_result r
        join federated.competition comp on comp.id = r.competition_id
        where r.competitor_id = c.id and comp.start_date >= competed_since and comp.start_date <= current_date
      ))
  )
  select option.facet, option.value, count(distinct c.id)::integer
  from candidates c
  cross join lateral (values
    ('type', c.competitor_type, c.age_matches and c.class_matches and c.discipline_matches),
    ('ageGroup', c.age_group_code, c.type_matches and c.class_matches and c.discipline_matches),
    ('class', c.class, c.type_matches and c.age_matches and c.discipline_matches),
    ('discipline', c.discipline, c.type_matches and c.age_matches and c.class_matches)
  ) option(facet, value, matches)
  where option.matches and option.value is not null
  group by option.facet, option.value
  order by option.facet, option.value;
$$;

comment on function federated.federation_competitor_facets is '@behavior +list -connection';
revoke all on function federated.federation_competitor_facets from public;
grant execute on function federated.federation_competitor_facets to anonymous;
--! EndIncluded functions/federation_competitor_facets.sql
--! Included functions/federation_event_facets.sql
drop function if exists federated.federation_event_facets;
drop type if exists federated.federation_event_facets;
drop type if exists federated.federation_event_year;

create type federated.federation_event_year as (
  year integer,
  event_count integer,
  mapped_event_count integer
);
create type federated.federation_event_facets as (
  years federated.federation_event_year[]
);

comment on column federated.federation_event_year.year is '@notNull';
comment on column federated.federation_event_year.event_count is '@notNull';
comment on column federated.federation_event_year.mapped_event_count is '@notNull';
comment on column federated.federation_event_facets.years is '@notNull';

create function federated.federation_event_facets(
  federation federated.federation, include_cancelled boolean default true
)
returns federated.federation_event_facets
language sql stable as $$
  select row(array(
    select row(
      extract(year from e.start_date)::integer,
      count(*) filter (where include_cancelled is distinct from false or e.status <> 'cancelled')::integer,
      count(*) filter (
        where (include_cancelled is distinct from false or e.status <> 'cancelled')
          and e.venue_lat is not null and e.venue_lng is not null
      )::integer
    )::federated.federation_event_year
    from federated.event e
    where e.federation = federation.code
    group by extract(year from e.start_date)
    order by extract(year from e.start_date) desc
  ))::federated.federation_event_facets;
$$;

revoke all on function federated.federation_event_facets from public;
grant execute on function federated.federation_event_facets to anonymous;
--! EndIncluded functions/federation_event_facets.sql
--! Included functions/person_age_group.sql
create or replace function person_age_group(p person) returns text
language sql stable as $$
  select code from federated.age_group
  where person_age @> (extract(year from current_date)::int - extract(year from p.birth_date)::int);
$$;

comment on function person_age_group is '@behavior -queryField';
grant execute on function person_age_group to anonymous;
--! EndIncluded functions/person_age_group.sql
--! Included functions/couple_age_group.sql
create or replace function couple_age_group(c couple) returns text
language sql stable as $$
  select r.age_group_code
  from person m, person w, federated.couple_age_rule r
  where m.id = c.man_id and w.id = c.woman_id
    and m.birth_date is not null and w.birth_date is not null
    and r.division = 'General'
    and r.age_group_code <> 'Under_21'
    and r.older_age @> (extract(year from current_date)::int - extract(year from least(m.birth_date, w.birth_date))::int)
    and r.younger_age @> (extract(year from current_date)::int - extract(year from greatest(m.birth_date, w.birth_date))::int)
  order by lower(r.older_age) desc nulls last, upper(r.older_age)
  limit 1;
$$;

comment on function couple_age_group is '@behavior -queryField';
grant execute on function couple_age_group to anonymous;
--! EndIncluded functions/couple_age_group.sql

COMMENT ON FUNCTION federated.class_rank(text) IS '@behavior -*';

do $$
begin
  if exists (
    select 1 from pg_attribute
    where attrelid = 'federated.competitor'::regclass
      and attname = 'external_id' and atttypid = 'bigint'::regtype
  ) then
    delete from federated.judge_score s
    using federated.competition_round r, federated.competition c
    where s.round_id = r.id and r.competition_id = c.id and c.federation = 'wdsf';

    delete from federated.competition_round_result where competitor_id like 'wdsf:%';
    delete from federated.competition_result where competitor_id like 'wdsf:%';
    delete from federated.competition_entry where competitor_id like 'wdsf:%';
    delete from federated.competitor_category_progress where competitor_id like 'wdsf:%';
    delete from federated.competitor_club_affiliation where competitor_id like 'wdsf:%';
    delete from federated.ranklist_entry where competitor_id like 'wdsf:%';
    delete from federated.competitor where federation = 'wdsf';

    -- Move the generated expression to a text column without dropping the ID
    -- column or its foreign keys. Other federations retain their existing IDs.
    alter table federated.competitor add column external_id_text text;
    update federated.competitor set external_id_text = external_id::text;
    alter table federated.competitor alter column external_id_text set not null;
    alter table federated.competitor alter column id
      set expression as (federation || ':' || external_id_text);
    alter table federated.competitor drop column external_id;
    alter table federated.competitor rename column external_id_text to external_id;
    alter table federated.competitor
      add constraint competitor_federation_external_id_key unique (federation, external_id);
    create index competitor_external_id_text_idx on federated.competitor (external_id);

    alter table federated.judge_score alter column competitor_id type text using competitor_id::text;

    update crawler.frontier set process_status = 'pending',
      last_process_error = null, last_process_error_at = null
    where federation = 'wdsf' and kind = 'participant'
      and last_successful_response_id is not null;
  end if;
end;
$$;

create index if not exists competition_start_date_idx
  on federated.competition (start_date) include (id);

--! Included views/competition_participation.sql
create or replace view federated.competition_participation
with (security_invoker = true) as
select competition_id, competitor_id from federated.competition_result
union all
select e.competition_id, e.competitor_id
from federated.competition_entry e
where not e.cancelled
  and not exists (
    select 1 from federated.competition_result r
    where (r.competition_id, r.competitor_id) = (e.competition_id, e.competitor_id)
  );

comment on view federated.competition_participation is '@primaryKey competition_id,competitor_id
@foreignKey (competition_id) references federated.competition (id)|@fieldName competition|@foreignFieldName participations
@foreignKey (competitor_id) references federated.competitor (id)|@fieldName competitor|@foreignFieldName competitionParticipations
@behavior -single -list -connection -insert -update -delete';
grant select on federated.competition_participation to anonymous;
--! EndIncluded views/competition_participation.sql
--! Included functions/tg_eir__sync_lesson_payment.sql
create or replace function app_private.tg_eir__sync_lesson_payment()
  returns trigger language plpgsql security definer
  set search_path = pg_catalog, public, pg_temp
as $$
declare
  instance_id bigint;
  instance event_instance;
  payment_id bigint;
begin
  for instance_id in select distinct unnest(array[old.instance_id, new.instance_id]) loop
    select * into instance from event_instance where id = instance_id for no key update;
    if not found then continue; end if;

    if (
      not exists (select 1 from payment where event_instance_id = instance.id)
      or array(select d.person_id from payment_debtor d join payment p on p.id = d.payment_id
          where p.event_instance_id = instance.id order by d.person_id)
        = array(select distinct r.person_id from event_instance_registration r
          where r.instance_id = instance.id and r.registration_status = 'active'
            and r.person_id is not null and not instance.is_cancelled order by r.person_id)
    ) then
      continue;
    end if;

    delete from payment where event_instance_id = instance.id;
    if not instance.is_cancelled and instance.type = 'lesson' and instance.since < now() then
      payment_id := (create_event_instance_payment(instance)).id;
      update payment set status = 'unpaid' where id = payment_id;
      perform resolve_payment_with_credit(payment.*) from payment where id = payment_id;
    end if;
  end loop;
  return null;
end;
$$;

drop trigger if exists _800_sync_lesson_payment on event_instance_registration;
create constraint trigger _800_sync_lesson_payment
  after insert or delete or update of instance_id, person_id, registration_status
  on event_instance_registration
  deferrable initially deferred for each row
  execute function app_private.tg_eir__sync_lesson_payment();
--! EndIncluded functions/tg_eir__sync_lesson_payment.sql
--! Included functions/create_event_instance_payment.sql
create or replace function create_event_instance_payment(i event_instance)
  returns payment
  language plpgsql
as $$
declare
  created_payment payment;
  duration numeric(19, 4);
begin
  if current_tenant_id() <> 2 then
    return null;
  end if;

  -- Serialize initial charging with registration changes and their deferred sync.
  select * into i from event_instance
  where id = i.id and tenant_id = current_tenant_id() for no key update;
  if not found or i.is_cancelled then return null; end if;

  select p.* into created_payment from payment p where p.event_instance_id = i.id;

  if found then
    return created_payment;
  end if;

  if i.type <> 'lesson' or not exists (
    select 1
    from event_instance_registration registration
    where registration.instance_id = i.id
      and registration.person_id is not null
      and registration.registration_status = 'active'
  ) then
    return null;
  end if;

  duration := extract(epoch from (i.until - i.since)) / 60;

  insert into payment (accounting_period_id, status, event_instance_id, due_at)
  values (
    (select id from accounting_period where tenant_id = current_tenant_id() and range @> now()),
    'tentative', i.id, now() + interval '2 weeks'
  )
  on conflict (event_instance_id) where event_instance_id is not null do nothing
  returning * into created_payment;

  if not found then
    return (select p from payment p where p.event_instance_id = i.id);
  end if;

  insert into payment_recipient (payment_id, account_id, amount)
  select distinct on (account.id)
    created_payment.id,
    account.id,
    trainer.member_price_45min_amount / 45 * duration
  from event_instance_trainers(i) trainer
  cross join lateral person_account(trainer.person_id, trainer.currency) account
  where trainer.tenant_id = current_tenant_id()
    and trainer.member_price_45min_amount is not null;

  insert into payment_debtor (payment_id, person_id)
  select distinct created_payment.id, registration.person_id
  from event_instance_registration registration
  where registration.instance_id = i.id
    and registration.person_id is not null
    and registration.registration_status = 'active';

  return created_payment;
end
$$;

COMMENT ON FUNCTION create_event_instance_payment IS '@omit';
GRANT ALL ON FUNCTION create_event_instance_payment TO anonymous;
--! EndIncluded functions/create_event_instance_payment.sql

insert into crawler.frontier (federation, kind, key)
select federation, 'couple', external_id from federated.competitor
where federation = 'wdsf' and competitor_type = 'couple'
on conflict (federation, kind, key) do nothing;

-- Older participant loads used positions in partial dance arrays as round-wide
-- positions. Reconstruct only inconsistent rounds from their cached participants.
WITH affected AS MATERIALIZED (
  SELECT DISTINCT rd.round_id, c.external_id
  FROM federated.round_dance rd
  JOIN federated.competition_round cr ON cr.id = rd.round_id
  JOIN federated.competition c ON c.id = cr.competition_id
  JOIN federated.category cat ON cat.id = c.category_id
  JOIN federated.dance_program_dance dpd
    ON dpd.program_id = rd.dance_program_id AND dpd.dance_code = rd.dance_code
  WHERE c.federation = 'wdsf' AND (
    rd.dance_order <> dpd.dance_order
    OR (cat.discipline = 'RHYTHM' AND rd.dance_code = 'OT')
  )
), participants AS (
  SELECT e->>'id' AS key
  FROM (SELECT DISTINCT external_id FROM affected) a
  JOIN crawler.frontier f
    ON f.federation = 'wdsf' AND f.kind = 'participantIndex' AND f.key = a.external_id
  JOIN crawler.json_response r ON r.id = f.last_successful_response_id
  JOIN crawler.json_response_cache cache USING (content_hash)
  CROSS JOIN LATERAL jsonb_array_elements(cache.content) e
), requeued AS (
  UPDATE crawler.frontier f
  SET process_status = 'pending', last_process_error = null, last_process_error_at = null
  FROM participants p
  WHERE f.federation = 'wdsf' AND f.kind = 'participant' AND f.key = p.key
    AND f.last_successful_response_id IS NOT NULL
)
DELETE FROM federated.competition_round cr USING affected a
WHERE cr.id = a.round_id;

UPDATE crawler.frontier
SET process_status = 'pending', last_process_error = null, last_process_error_at = null
WHERE federation = 'wdsf' AND kind = 'participant' AND process_status = 'error'
  AND last_successful_response_id IS NOT NULL
  AND (last_process_error LIKE '%judge_score_round_id_dance_order_dance_code_fkey%'
    OR last_process_error LIKE '%round_dance_dance_program_id_dance_code_fkey%'
    OR last_process_error LIKE '%competition_round_judge_round_id_judge_index_key%');

drop function if exists public.person_csts_candidates(public.person, integer, real);

--! Included functions/person_federation_candidates.sql
drop function if exists person_federation_candidates;
drop type if exists person_federation_candidates_record;

create type person_federation_candidates_record as (
  id integer,
  name text,
  age_group text,
  similarity real
);

comment on column person_federation_candidates_record.id is '@notNull';
comment on column person_federation_candidates_record.similarity is '@notNull';

create function person_federation_candidates(
  in_person person, federation text, "limit" integer default 10, threshold real default 0.4
) returns setof person_federation_candidates_record
language sql stable as $$
  select fp.external_id::integer, fp.canonical_name, fp.age_group_code,
    similarity(fp.search_name, in_person.search_name) as name_score
  from federated.person fp
  where fp.federation = $2
    and fp.external_id between 0 and 2147483647
    and fp.search_name % in_person.search_name
    and similarity(fp.search_name, in_person.search_name)
      >= greatest(0.3::real, least(coalesce(threshold, 0.4), 1::real))
  order by
    coalesce(fp.dob = in_person.birth_date, false) desc,
    name_score desc, fp.canonical_name, fp.external_id
  limit greatest(0, least(coalesce($3, 10), 50));
$$;

comment on function person_federation_candidates is '@simpleCollections only
@notNull';
grant execute on function person_federation_candidates to anonymous;
--! EndIncluded functions/person_federation_candidates.sql

create index if not exists event_instance_registration_active_person_idx
  on event_instance_registration (tenant_id, person_id)
  include (instance_id, status)
  where registration_status = 'active';

--! Included views/person_last_activity.sql
-- Owner privileges allow summaries of private records without granting access to
-- account links, card codes, or invitations. Keep caller/tenant checks here.
create or replace view person_last_activity
with (security_barrier = false) as
select
  (select current_tenant_id()) as tenant_id,
  p.id as person_id,
  web.web_at,
  card.card_entry_at,
  training.training_at,
  competition.competition_on
from person p
left join lateral (
  select max(u.last_active_at) as web_at
  from user_proxy up
  join users u on u.id = up.user_id
  where up.person_id = p.id and up.status = 'active'
) web on true
left join lateral (
  select e.occurred_at as card_entry_at
  from access_event e
  where e.tenant_id = (select current_tenant_id())
    and e.person_id = p.id and e.allowed and e.occurred_at <= now()
  order by e.occurred_at desc
  limit 1
) card on true
left join lateral (
  select max(e.since) as training_at
  from event_instance_registration r
  join event_instance e on e.id = r.instance_id and e.tenant_id = r.tenant_id
  where r.person_id = p.id and r.tenant_id = (select current_tenant_id())
    and r.registration_status = 'active'
    and (r.status = 'attended' or e.type = 'lesson')
    and not e.is_cancelled and e.since <= now()
    and e.type in ('lesson', 'group', 'camp')
) training on true
left join lateral (
  select max(c.start_date) as competition_on
  from (values ('csts', p.csts_id::bigint), ('wdsf', p.wdsf_id::bigint)) l(federation, external_id)
  join federated.person fp on (fp.federation, fp.external_id) = (l.federation, l.external_id)
  join federated.competitor_component cc on cc.person_id = fp.id
  join federated.competition_result r on r.competitor_id = cc.competitor_id
  join federated.competition c on c.id = r.competition_id
  where l.external_id <> 0 and c.start_date <= current_date
) competition on true
where (select pg_has_role(current_user, 'trainer', 'member'))
  and (
    exists (select from current_tenant_membership m where m.person_id = p.id)
    or exists (select from current_tenant_trainer t where t.person_id = p.id)
    or exists (select from current_tenant_administrator a where a.person_id = p.id)
    or exists (select from current_cohort_membership cm where cm.person_id = p.id)
  );

comment on view person_last_activity is '@primaryKey person_id
@unique tenant_id,person_id
@foreignKey (person_id) references public.person (id)|@foreignFieldName lastActivity
@behavior -query:resource:single -query:resource:list -query:resource:connection -insert -update -delete';
comment on column person_last_activity.tenant_id is '@notNull';
comment on column person_last_activity.person_id is '@notNull';

revoke all on public.person_last_activity from public;
grant select on public.person_last_activity to anonymous;
--! EndIncluded views/person_last_activity.sql

--! Included functions/federation_people_facets.sql
create or replace function federated.federation_people_facets(
  federation federated.federation,
  search text default null,
  age_groups text[] default null,
  genders text[] default null,
  nationalities text[] default null
) returns table(facet text, value text, count integer)
language sql stable as $$
  with candidates as (
    select
      coalesce(p.age_group_code, nullif(p.age_group, ''), 'unknown') as age_group,
      coalesce(p.gender::text, 'unknown') as gender,
      coalesce(nullif(p.nationality, ''), 'unknown') as nationality
    from federated.person p
    where p.federation = federation.code
      and (nullif(trim(search), '') is null or p.external_id::text = trim(search) or p.id = trim(search)
        or p.search_name like '%' || replace(replace(replace(
          app_private.normalize_name(trim(search)), '!', '!!'), '%', '!%'), '_', '!_') || '%' escape '!')
  ), matches as (
    select *,
      coalesce(cardinality(age_groups), 0) = 0 or age_group = any(age_groups) as age_matches,
      coalesce(cardinality(genders), 0) = 0 or gender = any(genders) as gender_matches,
      coalesce(cardinality(nationalities), 0) = 0 or nationality = any(nationalities) as nationality_matches
    from candidates
  )
  select option.facet, option.value, count(*)::integer
  from matches m
  cross join lateral (values
    ('ageGroup', m.age_group, m.gender_matches and m.nationality_matches),
    ('gender', m.gender, m.age_matches and m.nationality_matches),
    ('nationality', m.nationality, m.age_matches and m.gender_matches)
  ) option(facet, value, matches)
  where option.matches
  group by option.facet, option.value
  order by option.facet, option.value;
$$;

comment on function federated.federation_people_facets is '@behavior +list -connection';
revoke all on function federated.federation_people_facets from public;
grant execute on function federated.federation_people_facets to anonymous;
--! EndIncluded functions/federation_people_facets.sql
--! Included functions/person_facets.sql
drop function if exists app_private.person_membership_candidates;
drop function if exists person_facets;

create or replace function app_private.person_membership_candidates(membership_state text)
returns table(person_id bigint, in_state boolean, cohort_ids bigint[], is_trainer boolean, is_admin boolean)
language sql stable as $$
  with current_people as (
    select person_id from current_tenant_membership
    union select person_id from current_tenant_trainer
    union select person_id from current_tenant_administrator
  ), former_people as (
    select person_id from tenant_membership where tenant_id = current_tenant_id() and status = 'expired'
    union select person_id from tenant_trainer where tenant_id = current_tenant_id() and status = 'expired'
    union select person_id from tenant_administrator where tenant_id = current_tenant_id() and status = 'expired'
  ), trainers as (
    select person_id from current_tenant_trainer where membership_state = 'current'
    union all
    select person_id from tenant_trainer
    where membership_state = 'former' and tenant_id = current_tenant_id() and status = 'expired'
  ), admins as (
    select person_id from current_tenant_administrator where membership_state = 'current'
    union all
    select person_id from tenant_administrator
    where membership_state = 'former' and tenant_id = current_tenant_id() and status = 'expired'
  ), cohorts as (
    select person_id, cohort_id from current_cohort_membership where membership_state = 'current'
    union all
    select person_id, cohort_id from cohort_membership
    where membership_state = 'former' and tenant_id = current_tenant_id() and status = 'expired'
  )
  select p.id,
    case membership_state
      when 'current' then p.id in (select person_id from current_people)
      when 'former' then p.id in (select person_id from former_people)
        and p.id not in (select person_id from current_people)
      else false
    end,
    array(select distinct cohort_id from cohorts where person_id = p.id),
    p.id in (select person_id from trainers),
    p.id in (select person_id from admins)
  from person p;
$$;
revoke all on function app_private.person_membership_candidates from public;
grant execute on function app_private.person_membership_candidates to anonymous;

create or replace function person_facets(
  membership_states text[] default array['current'],
  in_cohorts bigint[] default null,
  is_trainer boolean default null,
  is_admin boolean default null
) returns table(facet text, value text, count integer)
language sql stable as $$
  with candidates as (
    select s.state, m.*,
      in_cohorts is null or case when cardinality(in_cohorts) = 0
        then cardinality(m.cohort_ids) = 0 else m.cohort_ids && in_cohorts end as cohort_matches,
       $3 is null or m.is_trainer = $3 as trainer_matches,
       $4 is null or m.is_admin = $4 as admin_matches
    from (values ('current'), ('former')) s(state)
    cross join lateral app_private.person_membership_candidates(s.state) m
    where m.in_state
  )
  select option.facet, option.value, count(distinct c.person_id)::integer
  from candidates c
  cross join lateral (
    select 'cohort' as facet, unnest(case when cardinality(c.cohort_ids) = 0
      then array['none'] else c.cohort_ids::text[] end) as value
    where (coalesce(cardinality(membership_states), 0) = 0 or c.state = any(membership_states)) and c.trainer_matches and c.admin_matches
    union all
    select 'trainer', c.is_trainer::text
    where (coalesce(cardinality(membership_states), 0) = 0 or c.state = any(membership_states)) and c.cohort_matches and c.admin_matches
    union all
    select 'admin', c.is_admin::text
    where (coalesce(cardinality(membership_states), 0) = 0 or c.state = any(membership_states)) and c.cohort_matches and c.trainer_matches
    union all
    select 'state', c.state
    where c.cohort_matches and c.trainer_matches and c.admin_matches
  ) option
  group by option.facet, option.value
  order by option.facet, option.value;
$$;
comment on function person_facets is '@behavior +list -connection';
revoke all on function person_facets from public;
grant execute on function person_facets to anonymous;
--! EndIncluded functions/person_facets.sql
--! Included functions/event_instances_for_range.sql
do $$
begin
  create type event_instance_range_scope as enum ('all', 'top_level', 'mine', 'relevant');
exception when duplicate_object then null;
end
$$;

drop function if exists event_instances_for_range;

create or replace function event_instances_for_range(
  only_type event_type,
  start_range timestamptz,
  end_range timestamptz = null,
  trainer_ids bigint[] = null,
  participant_ids bigint[] = null,
  parent_id bigint = null,
  scope event_instance_range_scope = 'all',
  location_ids bigint[] = null,
  only_types event_type[] = null
) returns setof event_instance as $$
  with mine as (
    select instance_id from event_instance_registration
    where person_id = any ((select current_person_ids())::bigint[]) and registration_status = 'active'
    union all
    select instance_id from event_instance_trainer
    where person_id = any ((select current_person_ids())::bigint[])
  )
  select i.*
  from event_instance i
  where i.tenant_id = current_tenant_id()
    and (only_type is null or i.type = only_type)
    and (only_types is null or i.type = any(only_types))
    and case
      when $6 is not null then i.parent_id = $6
        and (scope <> 'mine' or i.id in (select instance_id from mine))
      when scope = 'all' then true
      when scope = 'top_level' then i.parent_id is null
      when scope = 'mine' then i.id in (select instance_id from mine)
      when scope = 'relevant' then i.parent_id is null
        or i.id in (select instance_id from mine)
        or i.parent_id in (select instance_id from mine)
    end
    and i.since < coalesce(end_range, 'infinity'::timestamptz)
    and i.until > start_range
    and (trainer_ids is null
      or exists (select 1 from event_instance_trainer where instance_id = i.id and person_id = any (trainer_ids)))
    and (participant_ids is null
      or exists (select 1 from event_instance_registration where instance_id = i.id and person_id = any (participant_ids) and registration_status = 'active'))
    and (location_ids is null or i.location_id = any (location_ids))
  order by i.since
  ;
$$ stable language sql;

comment on function event_instances_for_range is '@simpleCollections only';
grant all on function event_instances_for_range to anonymous;
--! EndIncluded functions/event_instances_for_range.sql
--! Included functions/event_instance_facets.sql
create or replace function event_instance_facets(
  start_range timestamptz,
  end_range timestamptz,
  only_types event_type[] default null,
  trainer_ids bigint[] default null,
  participant_ids bigint[] default null,
  parent_id bigint default null,
  scope event_instance_range_scope default 'all',
  location_ids bigint[] default null
) returns table(facet text, value text, count integer)
language sql stable as $$
  with candidates as (
    select i.id, i.type, i.location_id,
      array(select distinct person_id from event_instance_trainer where instance_id = i.id) as trainers,
      array(select distinct person_id from event_instance_registration
        where instance_id = i.id and registration_status = 'active') as participants
    from event_instances_for_range(null, start_range, end_range,
      parent_id => $6, scope => $7) i
  ), matches as (
    select *,
      only_types is null or type = any(only_types) as type_matches,
      trainer_ids is null or trainers && trainer_ids as trainer_matches,
      participant_ids is null or participants && participant_ids as participant_matches,
      location_ids is null or location_id = any(location_ids) as location_matches
    from candidates
  )
  select option.facet, option.value, count(distinct m.id)::integer
  from matches m
  cross join lateral (
    select 'type' as facet, upper(m.type::text) as value
    where m.trainer_matches and m.participant_matches and m.location_matches
    union all
    select 'trainer', unnest(m.trainers)::text
    where m.type_matches and m.participant_matches and m.location_matches
    union all
    select 'participant', unnest(m.participants)::text
    where m.type_matches and m.trainer_matches and m.location_matches
    union all
    select 'location', m.location_id::text
    where m.type_matches and m.trainer_matches and m.participant_matches and m.location_id is not null
  ) option
  group by option.facet, option.value
  order by option.facet, option.value;
$$;
comment on function event_instance_facets is '@behavior +list -connection';
revoke all on function event_instance_facets from public;
grant execute on function event_instance_facets to anonymous;
--! EndIncluded functions/event_instance_facets.sql
