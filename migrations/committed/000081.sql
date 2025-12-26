--! Previous: sha1:4b839d8dd233a620605e3bc158d2bc8da56fd249
--! Hash: sha1:5281d8e058b51919d30bc69910213a58cc22cbdb

--! split: 1-current.sql
create or replace view current_cohort_membership as
  select * from cohort_membership where tenant_id = (select current_tenant_id()) and status = 'active';
create or replace view current_tenant_membership as
  select * from tenant_membership where tenant_id = (select current_tenant_id()) and status = 'active';
create or replace view current_tenant_trainer as
  select * from tenant_trainer where tenant_id = (select current_tenant_id()) and status = 'active';
create or replace view current_tenant_administrator as
  select * from tenant_administrator where tenant_id = (select current_tenant_id()) and status = 'active';

comment on view current_cohort_membership is '@omit';
comment on view current_tenant_membership is '@omit';
comment on view current_tenant_trainer is '@omit';
comment on view current_tenant_administrator is '@omit';

grant select on current_cohort_membership to anonymous;
grant select on current_tenant_membership to anonymous;
grant select on current_tenant_trainer to anonymous;
grant select on current_tenant_administrator to anonymous;

CREATE or replace FUNCTION public.filtered_people(
  is_trainer boolean,
  is_admin boolean,
  in_cohorts bigint[] default null,
  membership_state text default 'current'
) RETURNS SETOF person LANGUAGE sql STABLE AS $$
  select p.* from (
    select tm.person_id from tenant_membership tm
    where lower(coalesce(membership_state,'current')) = 'former' and tm.tenant_id = (select current_tenant_id()) and tm.status = 'expired'
      and not exists(select 1 from current_tenant_membership a where a.person_id = tm.person_id)
    union
    select tm.person_id from current_tenant_membership tm
    where lower(coalesce(membership_state,'current')) = 'current'
    union
    select tt.person_id from current_tenant_trainer tt
    where lower(coalesce(membership_state,'current')) = 'current'
    union
    select ta.person_id from current_tenant_administrator ta
    where lower(coalesce(membership_state,'current')) = 'current'
  ) vis
  join person p on p.id=vis.person_id
  where (
    lower(coalesce(membership_state,'current')) = 'former' and (
      in_cohorts is null or (
        cardinality(in_cohorts) = 0 and not exists(
          select 1 from cohort_membership cm where cm.person_id = p.id and cm.tenant_id = (select current_tenant_id()) and cm.status = 'expired')
      ) or (
        cardinality(in_cohorts) > 0 and exists(
          select 1 from cohort_membership cm where cm.person_id = p.id and cm.tenant_id = (select current_tenant_id()) and cm.status = 'expired' and cm.cohort_id = any(in_cohorts))
      )
    ) and (
      is_trainer is null or is_trainer = exists(
        select 1 from tenant_trainer tt where tt.person_id = p.id and tt.tenant_id = (select current_tenant_id()) and tt.status = 'expired')
    ) and (
      is_admin is null or is_admin = exists(
        select 1 from tenant_administrator ta where ta.person_id = p.id and ta.tenant_id = (select current_tenant_id()) and ta.status = 'expired')
    )
  ) or (
    lower(coalesce(membership_state,'current')) = 'current' and (
      in_cohorts is null or (
        cardinality(in_cohorts) = 0 and not exists(
          select 1 from current_cohort_membership cm where cm.person_id = p.id)
      ) or (
        cardinality(in_cohorts) > 0 and exists(
          select 1 from current_cohort_membership cm where cm.person_id = p.id and cm.cohort_id = any(in_cohorts))
      )
    ) and (
      is_trainer is null or is_trainer = exists(
        select 1 from current_tenant_trainer tt where tt.person_id = p.id)
    ) and (
      is_admin is null or is_admin = exists(
        select 1 from current_tenant_administrator ta where ta.person_id = p.id))
    );
$$;
COMMENT ON FUNCTION public.filtered_people IS '@simpleCollections only';
GRANT ALL ON FUNCTION public.filtered_people TO anonymous;

create or replace function app_private.create_jwt_token(u users) returns jwt_token
    language sql stable
as $$
with
  person_ids as (
    select distinct up.person_id from user_proxy up where up.user_id = u.id
  ),
  tenant_memberships as (
    select distinct tm.tenant_id from tenant_membership tm join person_ids p on p.person_id = tm.person_id where tm.status = 'active'
  ),
  tenant_trainers as (
    select distinct tt.tenant_id from tenant_trainer tt join person_ids p on p.person_id = tt.person_id where tt.status = 'active'
  ),
  tenant_admins as (
    select distinct ta.tenant_id from tenant_administrator ta join person_ids p on p.person_id = ta.person_id where ta.status = 'active'
  ),
  tenant_ids as (
    select tenant_id from tenant_memberships
    union
    select tenant_id from tenant_trainers
    union
    select tenant_id from tenant_admins
  ),
  cohort_ids as (
    select distinct cm.cohort_id from cohort_membership cm join person_ids p on p.person_id = cm.person_id where cm.status = 'active'
  ),
  couple_ids as (
    select distinct c.id from couple c join person_ids p on p.person_id = c.man_id where c.status = 'active'
    union all
    select distinct c.id from couple c join person_ids p on p.person_id = c.woman_id where c.status = 'active'
  )
  select
    extract(epoch from now() + interval '7 days')::integer as exp,
    u.id as user_id,
    (select current_tenant_id()) as tenant_id,
    u.u_login as username,
    u.u_email as email,
    coalesce(to_json((select array_agg(p.person_id) from person_ids p)), '[]'::json) as my_person_ids,
    coalesce(to_json((select array_agg(t.tenant_id) from tenant_ids t)), '[]'::json) as my_tenant_ids,
    coalesce(to_json((select array_agg(c.cohort_id) from cohort_ids c)), '[]'::json) as my_cohort_ids,
    coalesce(to_json((select array_agg(c.id) from couple_ids c)), '[]'::json) as my_couple_ids,
    exists (select 1 from tenant_ids t where t.tenant_id = (select current_tenant_id())) as is_member,
    exists (select 1 from tenant_trainers t where t.tenant_id = (select current_tenant_id())) as is_trainer,
    exists (select 1 from tenant_admins a where a.tenant_id = (select current_tenant_id())) as is_admin,
    app_private.is_system_admin(u.id) as is_system_admin;
$$;


drop function if exists app_private.visible_person_ids_array;
create or replace function app_private.visible_person_ids() returns setof bigint as $$
  -- always visible: trainers + admins in current tenant
  select person_id from current_tenant_trainer
  union all
  select person_id from current_tenant_administrator
  union all
  -- members visible only if viewer is in tenant (any role)
  select person_id from current_tenant_membership WHERE (SELECT current_tenant_id() = ANY (my_tenants_array()))
$$ language sql stable leakproof parallel safe;

grant all on function app_private.visible_person_ids() to anonymous;


create or replace function people_without_access_with_existing_account() returns setof person stable language sql as $$
select * from person
where not exists (select 1 from user_proxy where person_id = person.id)
  and not exists (select 1 from person_invitation where person_id = person.id)
  and exists (select 1 from users where users.u_email = person.email)
  and (exists (select 1 from current_tenant_membership where person_id = person.id)
  or exists (select 1 from current_tenant_trainer where person_id = person.id)
  or exists (select 1 from current_tenant_administrator where person_id = person.id));
$$;
grant all on function people_without_access_with_existing_account to anonymous;

COMMENT ON function people_without_access_with_existing_account IS '@simpleCollections only';

create or replace function people_without_access_or_invitation() returns setof person stable language sql as $$
select * from person
where not exists (select 1 from user_proxy where person_id = person.id)
  and not exists (select 1 from person_invitation where person_id = person.id)
  and not exists (select 1 from person_invitation where email = person.email)
  and not exists (select 1 from users where users.u_email = person.email)
  and (exists (select 1 from current_tenant_membership where person_id = person.id)
    or exists (select 1 from current_tenant_trainer where person_id = person.id)
    or exists (select 1 from current_tenant_administrator where person_id = person.id));
$$;
grant all on function people_without_access_or_invitation to anonymous;

COMMENT ON function people_without_access_or_invitation IS '@simpleCollections only';

create or replace function people_without_access_with_invitation() returns setof person stable language sql as $$
select * from person
where not exists (select 1 from user_proxy where person_id = person.id)
  and exists (select 1 from person_invitation where person_id = person.id)
  and (exists (select 1 from current_tenant_membership where person_id = person.id)
  or exists (select 1 from current_tenant_trainer where person_id = person.id)
  or exists (select 1 from current_tenant_administrator where person_id = person.id));
$$;
grant all on function people_without_access_with_invitation to anonymous;

COMMENT ON function people_without_access_with_invitation IS '@simpleCollections only';

CREATE or replace FUNCTION person_is_admin(p person) RETURNS boolean AS $$
  select exists (select 1 from current_tenant_administrator where person_id = p.id);
$$ language sql stable PARALLEL SAFE LEAKPROOF;
GRANT ALL ON FUNCTION person_is_admin TO anonymous;

CREATE or replace FUNCTION person_is_trainer(p person) RETURNS boolean AS $$
  select exists (select 1 from current_tenant_trainer where person_id = p.id);
$$ language sql stable PARALLEL SAFE LEAKPROOF;
GRANT ALL ON FUNCTION person_is_trainer TO anonymous;

CREATE or replace FUNCTION person_is_member(p person) RETURNS boolean AS $$
  select exists (select 1 from current_tenant_membership where person_id = p.id);
$$ language sql stable PARALLEL SAFE LEAKPROOF;
GRANT ALL ON FUNCTION person_is_member TO anonymous;


create or replace function app_private.queue_announcement_notifications(in_announcement_id bigint)
returns void
language plpgsql
security definer
as $$
declare
  v_is_visible boolean;
  v_since timestamptz;
  v_until timestamptz;
  v_user_ids bigint[];
begin
  select is_visible, scheduled_since, scheduled_until
  into v_is_visible, v_since, v_until
  from announcement
  where id = in_announcement_id;

  if not found then
    return;
  end if;

  if not (v_is_visible
      and (v_since is null or v_since <= now())
      and (v_until is null or v_until > now())) then
    return;
  end if;

  with role_flags as (
    select
      coalesce(bool_or(audience_role = 'member'), false) as has_member,
      coalesce(bool_or(audience_role = 'trainer'), false) as has_trainer,
      coalesce(bool_or(audience_role = 'administrator'), false) as has_administrator
    from announcement_audience
    where announcement_id = in_announcement_id
      and audience_role is not null
  ),
  role_people as (
    select distinct x.person_id
    from role_flags rf
    join lateral (
      select tm.person_id from current_tenant_membership tm where rf.has_member
      union all
      select tt.person_id from current_tenant_trainer tt where rf.has_trainer
      union all
      select ta.person_id from current_tenant_administrator ta where rf.has_administrator
    ) x on true
  ),
  role_users as (
    select distinct u.id as user_id
    from role_people rp
    join user_proxy up on up.person_id = rp.person_id and up.status = 'active'
    join users u on u.id = up.user_id
  ),
  cohort_users as (
    select distinct u.id as user_id
    from announcement_audience aa
    join current_cohort_membership cm on cm.cohort_id = aa.cohort_id
    join user_proxy up on up.person_id = cm.person_id and up.status = 'active'
    join users u on u.id = up.user_id
    where aa.announcement_id = in_announcement_id
      and aa.cohort_id is not null
  )
  select array_agg(distinct user_id order by user_id)
  into v_user_ids
  from (
    select user_id from role_users
    union
    select user_id from cohort_users
  ) recipients;

  if v_user_ids is null or array_length(v_user_ids, 1) = 0 then
    return;
  end if;

  perform graphile_worker.add_job(
    'notify_announcement',
    json_build_object(
      'announcement_id', in_announcement_id,
      'user_ids', v_user_ids
    )
  );
end;
$$;
select verify_function('app_private.queue_announcement_notifications');
comment on function app_private.queue_announcement_notifications is '@omit';
grant all on function app_private.queue_announcement_notifications to anonymous;

create or replace function app_private.tg_announcement__after_write()
returns trigger
language plpgsql
security definer
as $$
-- @plpgsql_check_options: oldtable = oldtable, newtable = newtable
declare
  rec record;
  old_row record;
  was_published boolean;
  is_published boolean;
begin
  for rec in
    select * from newtable
  loop
    is_published := coalesce(rec.is_visible, false)
      and (rec.scheduled_since is null or rec.scheduled_since <= now())
      and (rec.scheduled_until is null or rec.scheduled_until > now());

    if TG_OP = 'INSERT' then
      was_published := false;
    else
      select * into old_row
      from oldtable
      where id = rec.id;

      if not found then
        was_published := false;
      else
        was_published := coalesce(old_row.is_visible, false)
          and (old_row.scheduled_since is null or old_row.scheduled_since <= now())
          and (old_row.scheduled_until is null or old_row.scheduled_until > now());
      end if;
    end if;

    if is_published and not was_published then
      perform app_private.queue_announcement_notifications(rec.id);
    end if;
  end loop;

  return null;
end;
$$;
select verify_function('app_private.tg_announcement__after_write', 'announcement');

create or replace function app_private.tg_announcement_audience__after_write()
returns trigger
language plpgsql
security definer
as $$
-- @plpgsql_check_options: oldtable = oldtable, newtable = newtable
declare
  rec record;
begin
  if TG_OP = 'DELETE' then
    for rec in (
      select distinct announcement_id from oldtable
    ) loop
      perform app_private.queue_announcement_notifications(rec.announcement_id);
    end loop;
  else
    for rec in (
      select distinct announcement_id from newtable
    ) loop
      perform app_private.queue_announcement_notifications(rec.announcement_id);
    end loop;
  end if;
  return null;
end;
$$;
select verify_function('app_private.tg_announcement_audience__after_write', 'announcement_audience');

drop trigger if exists _600_notify_announcement_insert on announcement;
drop trigger if exists _600_notify_announcement_update on announcement;
create trigger _600_notify_announcement_insert
  after insert on announcement
  referencing new table as newtable
  for each statement execute function app_private.tg_announcement__after_write();
create trigger _600_notify_announcement_update
  after update on announcement
  referencing new table as newtable old table as oldtable
  for each statement execute function app_private.tg_announcement__after_write();

drop trigger if exists _600_notify_announcement_audience_insert on announcement_audience;
drop trigger if exists _600_notify_announcement_audience_update on announcement_audience;
create trigger _600_notify_announcement_audience_insert
  after insert on announcement_audience
  referencing new table as newtable
  for each statement execute function app_private.tg_announcement_audience__after_write();
create trigger _600_notify_announcement_audience_update
  after update on announcement_audience
  referencing new table as newtable old table as oldtable
  for each statement execute function app_private.tg_announcement_audience__after_write();

create or replace function tenant_couples(t tenant) returns setof couple as $$
  select couple.*
  from couple
  where couple.status = 'active'
    and (exists (select 1 from current_tenant_membership where person_id = man_id)
      or exists (select 1 from current_tenant_membership where person_id = woman_id));
$$ language sql stable;
grant all on function tenant_couples to anonymous;
comment on function tenant_couples is '@simpleCollections only';
