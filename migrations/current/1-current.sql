
--!include functions/event_instances_for_range.sql

drop function if exists person_active_couples(person);
drop function if exists person_all_couples(person);
drop function if exists tenant_couples(tenant);

comment on table couple is '@behavior +list -connection +query:resource:list -query:resource:connection
@ref federationCompetitors via:public.couple_federation_link;federated.competitor
@ref federationRanklistEntries via:public.couple_federation_link;federated.competitor;federated.ranklist_entry';

--!include functions/event_facets.sql
