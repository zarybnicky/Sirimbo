do $$
begin
  if exists (select 1 from pg_tables where schemaname = 'public' and tablename='skupiny') then
    alter table skupiny set schema app_private;
  end if;
end
$$;

GRANT ALL ON TABLE public.cohort TO anonymous;
ALTER TABLE public.cohort ENABLE ROW LEVEL SECURITY;
drop index if exists event_lesson_demand_registration_id_idx;
drop index if exists event_trainer_event_id_idx;

comment on column aktuality.at_kat is '@deprecated';
comment on column account.name is '@deprecated';
comment on column upozorneni.up_barvy is '@deprecated';
comment on column person.middle_name is '@deprecated';
