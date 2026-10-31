comment on table couple is '@behavior +list -connection +query:resource:list -query:resource:connection
@ref federationCompetitors via:public.couple_federation_link;federated.competitor
@ref federationRanklistEntries via:public.couple_federation_link;federated.competitor;federated.ranklist_entry';

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

--!include functions/event_registration_info.sql
--!include functions/event_facets.sql
--!include functions/set_event_registration.sql
--!include functions/event_trainer_lessons_remaining.sql
--!include functions/can_trainer_edit_event.sql
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
