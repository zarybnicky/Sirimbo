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
@ref couples via:public.couple_federation_link;public.couple';
COMMENT ON TABLE federated.competitor_component IS '@name federation_competitor_component
@behavior -insert -update -delete -list -query:resource:connection -query:resource:list -query:resource:single';
COMMENT ON TABLE federated.competitor_category_progress IS '@name federation_competitor_category_progress
@behavior -insert -update -delete -list -query:resource:connection -query:resource:list -query:resource:single';
COMMENT ON TABLE federated.competitor_club_affiliation IS '@name federation_competitor_club_affiliation
@behavior -insert -update -delete -list -query:resource:connection -query:resource:list -query:resource:single';
COMMENT ON TABLE federated.round_dance IS '@name competition_round_dance
@behavior -insert -update -delete -list -query:resource:connection -query:resource:list -query:resource:single';
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
@ref federationRanklistEntries via:public.person_federation_link;federated.person;federated.competitor_component;federated.competitor;federated.ranklist_entry';

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
@ref federationRanklistEntries via:public.couple_federation_link;federated.competitor;federated.ranklist_entry';

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

--!include functions/federation_competitor_facets.sql

ALTER TABLE federated.competition_official ADD COLUMN IF NOT EXISTS judge_label text;

--!include functions/competition_reports.sql
--!include functions/activity_timeline.sql

--!include views/crawler_frontier_summary.sql
--!include views/crawler_frontier_failure_group.sql

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
comment on constraint couple_age_rule_age_group_code_fkey on federated.couple_age_rule is '@fieldName canonicalAgeGroup
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

--!include functions/resolve_age_group.sql

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

--!include functions/tg_resolve_age_group.sql

create or replace trigger _resolve_age_group
  before insert or update of age_group on federated.category
  for each row execute function app_private.tg_resolve_age_group();
create or replace trigger _resolve_age_group
  before insert or update of age_group on federated.person
  for each row execute function app_private.tg_resolve_age_group();
create or replace trigger _resolve_age_group
  before insert or update of age_group on federated.competitor
  for each row execute function app_private.tg_resolve_age_group();

comment on column federated.category.age_group_code is '@behavior -insert -update';
comment on column federated.person.age_group_code is '@behavior -insert -update';
comment on column federated.competitor.age_group_code is '@behavior -insert -update';
comment on constraint category_age_group_code_fkey on federated.category is '@fieldName canonicalAgeGroup
@foreignFieldName categories
@foreignSimpleFieldName categories
@backwardBehavior +list -connection';
comment on constraint person_age_group_code_fkey on federated.person is '@fieldName canonicalAgeGroup
@backwardBehavior -single -list -connection';
comment on constraint competitor_age_group_code_fkey on federated.competitor is '@fieldName canonicalAgeGroup
@backwardBehavior -single -list -connection';

update federated.category set age_group_code = federated.resolve_age_group(age_group)
where age_group_code is distinct from federated.resolve_age_group(age_group);
update federated.person set age_group_code = federated.resolve_age_group(age_group)
where age_group_code is distinct from federated.resolve_age_group(age_group);
update federated.competitor set age_group_code = federated.resolve_age_group(age_group)
where age_group_code is distinct from federated.resolve_age_group(age_group);

--!include policies/crawler.sql

COMMENT ON FUNCTION federated.class_rank(text) IS '@behavior -*';
