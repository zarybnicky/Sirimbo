create or replace function sticky_announcements(
  order_by_updated boolean default false
) returns setof announcement
language sql stable
as $$
  with audience_claims as (
    select
      translate(coalesce(nullif(current_setting('jwt.claims.my_cohort_ids', true), ''), '[]'), '[]', '{}')::bigint[] as cohort_ids,
      coalesce(nullif(current_setting('jwt.claims.is_member', true), '')::boolean, false) as is_member,
      coalesce(nullif(current_setting('jwt.claims.is_trainer', true), '')::boolean, false) as is_trainer,
      coalesce(nullif(current_setting('jwt.claims.is_admin', true), '')::boolean, false) as is_admin
  )
  select announcement.*
  from announcement
  cross join audience_claims ac
  where is_visible = true
    and is_sticky = true
    and (scheduled_since is null or scheduled_since <= now())
    and (scheduled_until is null or scheduled_until >= now())
    and (
      not exists (
        select 1
        from announcement_audience aa_all
        where aa_all.announcement_id = announcement.id
      )
      or exists (
        select 1
        from announcement_audience aa
        where aa.announcement_id = announcement.id
          and (
            (aa.cohort_id is not null and aa.cohort_id = any (ac.cohort_ids))
            or (aa.audience_role = 'member' and ac.is_member)
            or (aa.audience_role = 'trainer' and ac.is_trainer)
            or (aa.audience_role = 'administrator' and ac.is_admin)
          )
      )
    )
  order by
    case when order_by_updated then updated_at else created_at end desc,
    created_at desc;
$$;
grant all on function sticky_announcements(order_by_updated boolean) to anonymous;

create or replace function my_announcements(
  sticky boolean default false,
  archive boolean default false,
  order_by_updated boolean default false
) returns setof announcement
  language sql stable
as $$
with audience_claims as (
  select
    (select array_agg(cohort_id) from current_cohort_membership cm where cm.person_id = any (current_person_ids())) as cohort_ids,
    (exists (select 1 from current_tenant_membership where person_id = any (current_person_ids()))) as is_member,
    (exists (select 1 from current_tenant_trainer where person_id = any (current_person_ids()))) as is_trainer,
    (exists (select 1 from current_tenant_administrator where person_id = any (current_person_ids()))) as is_admin
)
select a.*
from announcement a
cross join audience_claims ac
where a.is_sticky = sticky
  and a.is_visible = case when sticky then true else not archive end
  and (archive or (a.scheduled_since is null or a.scheduled_since <= now()))
  and (archive or (a.scheduled_until is null or a.scheduled_until >= now()))
  and (
    exists (
      select 1
      from announcement_audience aa
      where aa.announcement_id = a.id and (
        (aa.cohort_id is not null and aa.cohort_id = any (ac.cohort_ids))
          or (aa.audience_role = 'member' and ac.is_member)
          or (aa.audience_role = 'trainer' and ac.is_trainer)
          or (aa.audience_role = 'administrator' and ac.is_admin)
      )
    ) or not exists (
      select 1
      from announcement_audience aa_all
      where aa_all.announcement_id = a.id
    )
  )
order by
  case when order_by_updated then a.updated_at else a.created_at end desc,
  a.created_at desc;
$$;

grant all on function my_announcements to anonymous;
