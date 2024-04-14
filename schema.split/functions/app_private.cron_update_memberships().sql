CREATE FUNCTION app_private.cron_update_memberships() RETURNS void
    LANGUAGE sql
    AS $$
  update user_proxy set status = 'active' where now() <@ active_range and status <> 'active';
  update user_proxy set status = 'pending' where now() < since and status <> 'pending';
  update user_proxy set status = 'expired' where now() > until and status <> 'expired';

  update couple set status = 'active' where now() <@ active_range and status <> 'active';
  update couple set status = 'pending' where now() < since and status <> 'pending';
  update couple set status = 'expired' where now() > until and status <> 'expired';

  update cohort_membership set status = 'active' where now() <@ active_range and status <> 'active';
  update cohort_membership set status = 'pending' where now() < since and status <> 'pending';
  update cohort_membership set status = 'expired' where now() > until and status <> 'expired';

  update tenant_membership set status = 'active' where now() <@ active_range and status <> 'active';
  update tenant_membership set status = 'pending' where now() < since and status <> 'pending';
  update tenant_membership set status = 'expired' where now() > until and status <> 'expired';

  update tenant_trainer set status = 'active' where now() <@ active_range and status <> 'active';
  update tenant_trainer set status = 'pending' where now() < since and status <> 'pending';
  update tenant_trainer set status = 'expired' where now() > until and status <> 'expired';

  update tenant_administrator set status = 'active' where now() <@ active_range and status <> 'active';
  update tenant_administrator set status = 'pending' where now() < since and status <> 'pending';
  update tenant_administrator set status = 'expired' where now() > until and status <> 'expired';
$$;

GRANT ALL ON FUNCTION app_private.cron_update_memberships() TO administrator;


