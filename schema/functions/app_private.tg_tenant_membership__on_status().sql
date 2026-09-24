CREATE FUNCTION app_private.tg_tenant_membership__on_status() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
begin
  if new.status = 'expired' and not exists (
    select from tenant_membership
    where id <> new.id
      and tenant_id = new.tenant_id
      and person_id = new.person_id
      and active_range @> new.until
  ) then
    update cohort_membership
    set status = 'expired', until = new.until
    where tenant_id = new.tenant_id
      and person_id = new.person_id
      and since < new.until
      and (until is null or until > new.until);
  end if;
  return new;
end;
$$;
