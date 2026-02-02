--! Previous: sha1:79d881d37b1d0a478150aa8d1ec4aa0f64020334
--! Hash: sha1:fc8947c872a709fd79fae0584b451b4b12c6e4ed

--! split: 1-current.sql
drop function if exists create_event;


create or replace function tenant_administrator_tenant_name(tenant_administrator) returns text as $$
  select name from tenant where id = $1.tenant_id;
$$ language sql stable;

create or replace function tenant_trainer_tenant_name(tenant_trainer) returns text as $$
  select name from tenant where id = $1.tenant_id;
$$ language sql stable;

create or replace function tenant_membership_tenant_name(tenant_membership) returns text as $$
  select name from tenant where id = $1.tenant_id;
$$ language sql stable;

select app_private.drop_policies('public.cohort');
create policy my_tenant on cohort as restrictive using (tenant_id = (select current_tenant_id()));
CREATE POLICY admin_all ON cohort TO administrator USING (true);
CREATE POLICY all_view ON cohort FOR SELECT USING (true);

select app_private.drop_policies('public.cohort_membership');
create policy my_tenant on cohort_membership as restrictive using (tenant_id = (select current_tenant_id()));
CREATE POLICY admin_all ON public.cohort_membership TO administrator USING (true);
CREATE POLICY view_all ON public.cohort_membership FOR SELECT USING (true);

do
$$
  begin
    if not exists (select 1 from pg_constraint where conname = 'tenant_location_tenant_id_id_key') then
      ALTER TABLE public.tenant_location ADD CONSTRAINT tenant_location_tenant_id_id_key UNIQUE (tenant_id, id);
    end if;
  end;
$$;

ALTER TABLE public.event
  drop constraint if exists event_location_id_fkey,
  ADD CONSTRAINT event_location_id_fkey
    FOREIGN KEY (tenant_id, location_id)
      REFERENCES public.tenant_location (tenant_id, id)
      ON UPDATE CASCADE
      ON DELETE SET NULL;

comment on constraint event_location_id_fkey
  on public.event
  is E'@fieldName location
@foreignFieldName events';

ALTER TABLE public.event_instance
  drop constraint if exists event_instance_location_fkey,
  ADD CONSTRAINT event_instance_location_fkey
    FOREIGN KEY (tenant_id, location_id)
      REFERENCES public.tenant_location (tenant_id, id)
      ON UPDATE CASCADE
      ON DELETE SET NULL;

comment on constraint event_instance_location_fkey
  on public.event_instance
  is E'@fieldName location
@foreignFieldName eventInstances';
