--! Previous: sha1:fc8947c872a709fd79fae0584b451b4b12c6e4ed
--! Hash: sha1:69ede70235bd94cc2f045b45b9a126fd72d3763d

--! split: 1-current.sql
COMMENT ON TABLE public.form_responses IS E'@omit create,update,delete';

COMMENT ON TABLE public.event IS E'@omit create,delete';

COMMENT ON TABLE public.event_registration IS E'@omit create,update,delete
@behavior -query:resource:list -query:resource:connection -query:resource:single
@simpleCollections both';

COMMENT ON TABLE public.event_external_registration IS E'@omit update
@behavior -query:resource:list -query:resource:connection -query:resource:single
@simpleCollections only';

COMMENT ON TABLE public.event_target_cohort IS E'@omit create,update,delete
@behavior -query:resource:list -query:resource:connection -query:resource:single
@simpleCollections only';

COMMENT ON TABLE public.event_lesson_demand IS E'@omit create,update,delete
@behavior -query:resource:list -query:resource:connection -query:resource:single
@simpleCollections only';

COMMENT ON TABLE public.event_instance IS E'@omit create,delete
@simpleCollections only';

COMMENT ON TABLE public.event_trainer IS E'@omit create,update,delete
@behavior -query:resource:list -query:resource:connection -query:resource:single
@simpleCollections only';

COMMENT ON TABLE public.event_instance_trainer IS E'@omit create,update,delete
@behavior -query:resource:list -query:resource:connection -query:resource:single
@simpleCollections only';

COMMENT ON TABLE public.event_attendance IS E'@omit create,update,delete
@behavior -query:resource:list -query:resource:connection -query:resource:single
@simpleCollections only';

COMMENT ON TABLE public.announcement_audience IS E'@omit create,update,delete
@behavior -query:resource:list -query:resource:connection -query:resource:single';

COMMENT ON TABLE public.account IS '@omit create,update,delete
@behavior -query:resource:single
@simpleCollections only';

COMMENT ON TABLE public.posting IS '@omit create,update,delete
@behavior -query:resource:list -query:resource:connection -query:resource:single
@simpleCollections only';

COMMENT ON TABLE public.cohort_subscription IS '@omit create,update,delete
@behavior -query:resource:list -query:resource:connection -query:resource:single
@simpleCollections only';

COMMENT ON TABLE public.user_proxy IS '@simpleCollections only';

COMMENT ON TABLE public.tenant_membership IS '@simpleCollections only';

COMMENT ON TABLE public.tenant_trainer IS '@simpleCollections only';

COMMENT ON TABLE public.tenant_administrator IS '@simpleCollections only';

COMMENT ON TABLE public.cohort_membership IS '@simpleCollections only';

COMMENT ON TABLE public.announcement IS '@omit create';

COMMENT ON TABLE public.cohort_group IS '@omit create,update,delete';

COMMENT ON TABLE public.tenant IS '@omit create,delete
@behavior -singularRelation:resource:single
@simpleCollections only';
