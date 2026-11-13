/* @name CalendarFeedOwner */
select s.id, s.name, s.cross_tenant, owner_tenant.name as tenant_name,
  (s.condition).with_competitions,
  (s.condition).with_birthdays,
  (s.condition).can_register,
  array(select t.id::text from tenant t where s.cross_tenant or t.id = s.tenant_id order by t.id) as tenant_ids,
  to_jsonb(app_private.create_jwt_token(u)) as claims
from event_saved_search s
join users u on u.id = s.user_id
join tenant owner_tenant on owner_tenant.id = s.tenant_id
where s.feed_key = :key!;
