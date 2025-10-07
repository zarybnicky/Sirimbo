CREATE FUNCTION app_private.queue_announcement_notifications(in_announcement_id bigint) RETURNS void
    LANGUAGE plpgsql SECURITY DEFINER
    AS $$
declare
  v_tenant_id bigint;
  v_is_visible boolean;
  v_since timestamptz;
  v_until timestamptz;
  v_user_ids bigint[];
begin
  select tenant_id,
         coalesce(is_visible, false),
         scheduled_since,
         scheduled_until
  into v_tenant_id,
    v_is_visible,
    v_since,
    v_until
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
    select distinct ad.person_id
    from auth_details ad
    join role_flags rf on rf.has_member or rf.has_trainer or rf.has_administrator
    where (
      rf.has_member and v_tenant_id = any (coalesce(ad.tenant_memberships, '{}'::bigint[]))
    ) or (
      rf.has_trainer and v_tenant_id = any (coalesce(ad.tenant_trainers, '{}'::bigint[]))
    ) or (
      rf.has_administrator and v_tenant_id = any (coalesce(ad.tenant_administrators, '{}'::bigint[]))
    )
  ),
  role_users as (
    select distinct u.id as user_id
    from role_people rp
    join user_proxy up on up.person_id = rp.person_id and up.active
    join users u on u.id = up.user_id
    where u.tenant_id = v_tenant_id
  ),
  cohort_users as (
    select distinct u.id as user_id
    from announcement_audience aa
    join cohort_membership cm
      on cm.cohort_id = aa.cohort_id
     and cm.active
    join user_proxy up on up.person_id = cm.person_id and up.active
    join users u on u.id = up.user_id
    where aa.announcement_id = in_announcement_id
      and aa.cohort_id is not null
      and aa.tenant_id = v_tenant_id
      and cm.tenant_id = v_tenant_id
      and u.tenant_id = v_tenant_id
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

COMMENT ON FUNCTION app_private.queue_announcement_notifications(in_announcement_id bigint) IS '@omit';

GRANT ALL ON FUNCTION app_private.queue_announcement_notifications(in_announcement_id bigint) TO anonymous;
