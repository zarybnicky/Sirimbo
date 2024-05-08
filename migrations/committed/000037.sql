--! Previous: sha1:f64ae8d9c69732d1efa4c0fe976d03959d3883cd
--! Hash: sha1:f2506e112be72df6619056b53bfee52a50d259de

--! split: 1-current.sql
alter table event_target_cohort
  drop constraint event_target_cohort_cohort_id_fkey;
alter table cohort_subscription
  drop constraint cohort_subscription_cohort_id_fkey;
alter table cohort_membership
  drop constraint cohort_membership_cohort_id_fkey;
alter table platby_group_skupina
  drop constraint platby_group_skupina_pgs_id_skupina_fkey;
alter table upozorneni_skupiny
  drop constraint upozorneni_skupiny_ups_id_skupina_fkey;

do $$
begin
  if exists (select 1 from pg_views where viewname = 'cohort') then
    drop view if exists cohort;
  end if;

  if exists (select 1 from pg_Tables where tablename = 'skupiny') and exists (select 1 from pg_tables where tablename = 'cohort') then
    drop table if exists cohort;
  end if;
end
$$;

create table if not exists cohort (
    id bigint NOT NULL GENERATED ALWAYS AS IDENTITY PRIMARY KEY,
    tenant_id bigint DEFAULT public.current_tenant_id() NOT NULL REFERENCES tenant(id) ON DELETE CASCADE,
    cohort_group_id bigint REFERENCES cohort_group(id) ON DELETE SET NULL,
    name text NOT NULL,
    description text DEFAULT ''::text NOT NULL,
    color_rgb text NOT NULL,
    location text DEFAULT ''::text NOT NULL,
    is_visible boolean DEFAULT true NOT NULL,
    ordering integer DEFAULT 1 NOT NULL
);

insert into cohort
  (id, tenant_id, cohort_group_id, name, description, color_rgb, location, is_visible, ordering)
overriding system value
select
  s_id, tenant_id, cohort_group, s_name, s_description, s_color_rgb, s_location, s_visible, ordering
from skupiny;

COMMENT ON TABLE cohort is '@simpleCollections only';
GRANT ALL ON TABLE public.skupiny TO anonymous;
ALTER TABLE public.skupiny ENABLE ROW LEVEL SECURITY;

select app_private.drop_policies('public.cohort');
CREATE POLICY admin_all ON cohort TO administrator USING (true) WITH CHECK (true);
CREATE POLICY all_view ON cohort FOR SELECT USING (true);

COMMENT ON TABLE public.event_target_cohort IS E'@omit create,update,delete
@simpleCollections only';
COMMENT ON TABLE public.cohort_membership IS E'@simpleCollections only';
COMMENT ON TABLE public.cohort_subscription IS '@omit create,update,delete
@simpleCollections only';
COMMENT ON TABLE public.upozorneni_skupiny IS E'@omit create,update,delete';

alter table event_target_cohort
  add constraint event_target_cohort_cohort_id_fkey FOREIGN KEY (cohort_id) REFERENCES cohort (id);
alter table cohort_membership
  add constraint cohort_membership_cohort_id_fkey FOREIGN KEY (cohort_id) REFERENCES cohort (id);
alter table cohort_subscription
  add constraint cohort_subscription_cohort_id_fkey FOREIGN KEY (cohort_id) REFERENCES cohort (id);
alter table platby_group_skupina
  add constraint platby_group_skupina_pgs_id_skupina_fkey FOREIGN KEY (pgs_id_skupina) REFERENCES cohort (id);
alter table upozorneni_skupiny
  add constraint upozorneni_skupiny_ups_id_skupina_fkey foreign key (ups_id_skupina) references cohort (id);

-- alter table skupiny set schema app_private;
