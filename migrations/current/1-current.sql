drop function if exists public.filtered_people(boolean, boolean, bigint[], text);

comment on table public.dokumenty is '@omit';

comment on table public.cohort_membership is '@simpleCollections only
@behavior -query:resource:list -query:resource:connection';

comment on table public.couple is '@simpleCollections only
@behavior -query:resource:list -query:resource:connection';

comment on table public.event_instance is '@omit create,delete
@simpleCollections only
@behavior -query:resource:list -query:resource:connection';

comment on table public.payment is '@omit create
@simpleCollections only
@behavior -query:resource:list -query:resource:connection';

comment on table public.payment_debtor is '@omit create,update,delete
@simpleCollections only';

comment on table public.payment_recipient is '@omit create,update,delete
@simpleCollections only
@behavior -query:resource:list -query:resource:connection';

comment on table public.person_invitation is '@omit update
@simpleCollections only
@behavior -query:resource:list -query:resource:connection';

comment on table public.scoreboard_manual_adjustment is '@simpleCollections only
@behavior -query:resource:list -query:resource:connection';

comment on table public.tenant is '@omit create,delete
@behavior -singularRelation:resource:single -query:resource:list -query:resource:connection
@simpleCollections only';

comment on table public.tenant_administrator is '@simpleCollections only
@behavior -query:resource:list -query:resource:connection';

comment on table public.tenant_location is '@simpleCollections only
@behavior -query:resource:list -query:resource:connection';

comment on table public.tenant_membership is '@simpleCollections only
@behavior -query:resource:list -query:resource:connection';

comment on table public.tenant_settings is '@simpleCollections only
@omit create,delete
@behavior -query:resource:list -query:resource:connection';

comment on table public.tenant_trainer is '@simpleCollections only
@behavior -query:resource:list -query:resource:connection';

comment on table public.user_proxy is '@simpleCollections only
@behavior -query:resource:list -query:resource:connection';
