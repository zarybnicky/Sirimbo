--!include functions/event_instances_for_range.sql

drop function if exists person_active_couples(person);
drop function if exists person_all_couples(person);
drop function if exists tenant_couples(tenant);

comment on table couple is '@behavior +list -connection +query:resource:list -query:resource:connection
@ref federationCompetitors via:public.couple_federation_link;federated.competitor
@ref federationRanklistEntries via:public.couple_federation_link;federated.competitor;federated.ranklist_entry';

--!include functions/event_facets.sql
--!include functions/set_event_registration.sql
--!include functions/event_trainer_lessons_remaining.sql

drop function if exists event_remaining_person_spots(event);
drop function if exists event_instance_remaining_person_spots;
drop function if exists event_instance_trainer_lessons_remaining;
drop function if exists people_without_access_or_invitation();
drop function if exists people_without_access_with_existing_account();
drop function if exists people_without_access_with_invitation();
