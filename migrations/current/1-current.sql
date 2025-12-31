
delete from event_instance where until = since;
delete from event where not exists (select 1 from event_instance ei where ei.event_id = event.id);

alter table event_instance
  drop constraint if exists event_instance_until_gt_since,
  add constraint event_instance_until_gt_since check (until > since);

