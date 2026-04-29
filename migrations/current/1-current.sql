drop function if exists event_instances_for_range(only_type event_type, start_range timestamp with time zone, end_range timestamp with time zone, trainer_ids bigint[], only_mine boolean);

--!include functions/event_instances_for_range.sql
