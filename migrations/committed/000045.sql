--! Previous: sha1:07e9d6c2d390b9ee2eedd7ee6edaa94c30c9e24e
--! Hash: sha1:6d70a5dbb92e5acc2a9d1fa07b49194ea811e0b9

--! split: 1-current.sql
select app_private.drop_policies('public.event_attendance');
create policy current_tenant on event_attendance as restrictive using (tenant_id = (select current_tenant_id()));
CREATE POLICY admin_all ON event_attendance TO administrator USING (true);
CREATE POLICY admin_trainer_insert ON event_attendance for insert TO trainer with check (exists (
  select 1
  from event_instance
  left join event_trainer on event_instance.event_id=event_trainer.event_id
  left join event_instance_trainer on event_instance.id=event_instance_trainer.instance_id
  where event_attendance.instance_id=event_instance.id and (
    event_instance_trainer.person_id = any (current_person_ids())
    or event_trainer.person_id = any (current_person_ids())
  )
));
CREATE POLICY admin_trainer ON event_attendance for update TO trainer USING (exists (
  select 1
  from event_instance
  left join event_trainer on event_instance.event_id=event_trainer.event_id
  left join event_instance_trainer on event_instance.id=event_instance_trainer.instance_id
  where event_attendance.instance_id=event_instance.id and (
    event_instance_trainer.person_id = any (current_person_ids())
    or event_trainer.person_id = any (current_person_ids())
  )
));
CREATE POLICY view_visible_event ON event_attendance FOR SELECT USING (EXISTS (SELECT 1
   FROM event_instance
  WHERE event_attendance.instance_id = event_instance.id));

CREATE or replace FUNCTION person_account(p_id bigint, c text, OUT acc account) RETURNS account LANGUAGE plpgsql AS $$
begin
  select * into acc from account
  where person_id=p_id and currency=c and tenant_id=current_tenant_id();

  if not found then
    insert into account (tenant_id, person_id, currency)
    values (current_tenant_id(), p_id, coalesce(c, 'CZK'))
    on conflict on constraint account_tenant_id_person_id_currency_idx do nothing
    returning * into acc;
  end if;
end;
$$ security definer;

select verify_function('person_account');
grant all on function person_account to anonymous;

CREATE or replace FUNCTION tenant_account(c text, OUT acc account) RETURNS account LANGUAGE plpgsql AS $$
begin
  select * into acc from account
  where person_id is null and currency=c and tenant_id=current_tenant_id();

  if not found then
    insert into account (tenant_id, person_id, currency)
    values (current_tenant_id(), null, coalesce(c, 'CZK'))
    on conflict on constraint account_tenant_id_person_id_currency_idx do nothing
    returning * into acc;
  end if;
end;
$$ security definer;

select verify_function('tenant_account');
grant all on function tenant_account to anonymous;
