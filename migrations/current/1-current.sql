comment on table couple is '@behavior +list -connection +query:resource:list -query:resource:connection
@ref federationCompetitors via:public.couple_federation_link;federated.competitor
@ref federationRanklistEntries via:public.couple_federation_link;federated.competitor;federated.ranklist_entry';

comment on table federated.competition_result is '@name competition_result
@behavior -insert -update -delete -list +query:resource:connection -query:resource:list -query:resource:single
@foreignKey (competition_id,last_round) references federated.competition_round (competition_id,round_key)|@fieldName round|@behavior -manyRelation:resource:connection -manyRelation:resource:list';


drop function if exists person_active_couples(person);
drop function if exists person_all_couples(person);
drop function if exists tenant_couples(tenant);
drop function if exists event_remaining_person_spots(event);
drop function if exists event_instance_remaining_person_spots;
drop function if exists event_instance_trainer_lessons_remaining;
drop function if exists people_without_access_or_invitation();
drop function if exists people_without_access_with_existing_account();
drop function if exists people_without_access_with_invitation();
drop function if exists event_trainer_name(event_trainer);
drop function if exists person_has_access(person);
drop function if exists event_instance_trainers;
drop function if exists app_private.event_instance_trainers_at;
drop function if exists app_private.event_trainers_at(event, timestamptz);

do $$
begin
  if to_regtype('public.event_stats') is null then
    create type event_stats as (
      total integer,
      unknown integer,
      attended integer,
      not_excused integer
    );
  end if;

  if exists (select from pg_attribute
    where attrelid = 'event'::regclass and attname = 'stats' and atttypid = 'jsonb'::regtype) then
    alter table event alter column stats drop default;
    alter table event alter column stats type event_stats using row(
      coalesce((stats->>'TOTAL')::integer, 0),
      coalesce((stats->>'UNKNOWN')::integer, 0),
      coalesce((stats->>'ATTENDED')::integer, 0),
      coalesce((stats->>'NOT_EXCUSED')::integer, 0)
    )::event_stats;
  end if;
end $$;

alter table event alter column stats set default row(0, 0, 0, 0)::event_stats;
comment on column event.stats is
  '@behavior -attribute:insert -attribute:update -attribute:filterBy -attribute:orderBy';
comment on column event.manager_person_ids is '@behavior -attribute:insert -attribute:update';

comment on column event_stats.total is '@notNull';
comment on column event_stats.unknown is '@notNull';
comment on column event_stats.attended is '@notNull';
comment on column event_stats.not_excused is '@notNull';

--!include functions/event_stats.sql
--!include functions/event_registration_info.sql
--!include functions/event_facets.sql
drop function if exists public.activity_timeline;
drop view if exists public.activity_timeline_item;
drop type if exists public.activity_timeline_kind;
--!include functions/person_birthday.sql
--!include functions/set_event_registration.sql
--!include functions/event_trainer_lessons_remaining.sql
--!include functions/can_trainer_edit_event.sql
--!include functions/tg_event__refresh_manager_person_ids.sql
--!include functions/trainer_group_attendance_completion.sql
--!include functions/event_trainers.sql
--!include functions/event_approx_price.sql
--!include functions/create_event_payment.sql
--!include functions/create_next_cohort_subscription_payment.sql
--!include functions/create_missing_cohort_subscription_payments.sql
--!include functions/archive_cohort.sql
--!include functions/create_jwt_token.sql
--!include functions/tg_tenant_membership__on_status.sql
--!include functions/security_events.sql
--!include functions/scoreboard.sql
--!include functions/refresh_wdsf_round_dances.sql
--!include functions/refresh_competition_round_results.sql

drop function if exists federated.competitor_result_highlights;
drop function if exists federated.person_result_highlights;
drop function if exists federated.person_result_highlights;
drop function if exists app_private.result_highlights;
drop function if exists federated.result_highlights;
drop type if exists federated.result_highlight;

comment on table federated.person is '@name federation_person
@behavior -insert -update -delete -list
@ref people via:public.person_federation_link;public.person behavior:"+list -connection"
@ref competitionResults via:federated.competitor_component;federated.competitor;federated.competition_result behavior:"+list -connection"';

comment on constraint competition_result_competitor_id_fkey on federated.competition_result is null;

comment on table federated.competitor is '@name federation_competitor
@behavior -insert -update -delete -list
@ref people via:federated.competitor_component;federated.person;public.person_federation_link;public.person behavior:"+list -connection"
@ref couples via:public.couple_federation_link;public.couple
@ref competitions via:federated.competition_participation;federated.competition
@ref competitionResults via:federated.competition_result behavior:"+list -connection"';

do $$
begin
  perform app_private.refresh_event_stats(id) from event where type = 'group' order by id;
end
$$;

alter table federated.judge_score
  alter constraint judge_score_round_id_dance_order_dance_code_fkey deferrable initially immediate;

comment on table posting is '@behavior +list +connection -insert -update -delete -query:resource:list -query:resource:connection -query:resource:single';

comment on table payment is '@behavior +list +connection -insert -query:resource:list +query:resource:connection';

do $$
begin
  create type payment_kind as enum ('event', 'cohort-subscription', 'other');
exception when duplicate_object then null;
end
$$;

alter table payment add column if not exists kind payment_kind
  generated always as (
    case
      when event_id is not null then 'event'::payment_kind
      when cohort_subscription_id is not null then 'cohort-subscription'::payment_kind
      else 'other'::payment_kind
    end
  ) stored;

update federated.competition c
set competition_type = 'ranking'
from federated.category cat
where cat.id = c.category_id
  and c.federation = 'wdsf'
  and cat.class = 'GRAND SLAM'
  and c.competition_type is distinct from 'ranking'::federated.competition_type;

insert into crawler.frontier (federation, kind, key)
  select federation, 'competitionEventIndex' as kind, key
  from crawler.frontier
  where federation = 'csts' and kind = 'eventIndex'
  on conflict (federation, kind, key) do nothing;

insert into crawler.rate_limit_rule (host, max_requests, per_interval)
values ('szts.ksis.eu', 1, interval '5 seconds')
on conflict (host) do nothing;

do $$
declare competition_ids bigint[];
declare competition_id bigint;
begin
  with corrected as (
    update federated.competition_result result
    set last_round = reached.name, last_dance = reached.dance
    from crawler.frontier frontier
    join crawler.json_response response on response.id = frontier.last_successful_response_id
    join crawler.json_response_cache cache on cache.content_hash = response.content_hash
    join federated.competition competition
      on competition.federation = 'wdsf' and competition.external_id = cache.content->>'competitionId'
    cross join lateral (
      select round.value->>'name' as name, round.value->'dances'->-1->>'name' as dance
      from jsonb_array_elements(cache.content->'rounds') with ordinality as round(value, position)
      order by case when round.value->>'name' ~* '^f(inal)?$' then 2
        when round.value->>'name' = 'R' then 0 else 1 end desc, position desc
      limit 1
    ) reached
    where frontier.federation = 'wdsf' and frontier.kind = 'participant'
      and result.competition_id = competition.id
      and result.competitor_id = 'wdsf:' || (cache.content->>'coupleId')
      and result.last_round = 'R' and reached.name <> 'R'
    returning result.competition_id
  )
  select array_agg(distinct corrected.competition_id) into competition_ids from corrected;

  foreach competition_id in array coalesce(competition_ids, '{}'::bigint[]) loop
    perform app_private.refresh_competition_round_results(competition_id);
  end loop;
end $$;
