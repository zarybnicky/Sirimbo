comment on table public.event_instance is E'@omit create,delete\n@simpleCollections only';
comment on table public.event_series is
  E'@behavior -query:resource:list -query:resource:connection';

drop function if exists public.detach_event_instance(bigint, text);
