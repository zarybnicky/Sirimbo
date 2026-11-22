/* @name AccessControlContext */
select tenant_id::text as "tenantId!" from tenant_settings
where settings->>'accessCredentialToken' = :token! limit 2;

/* @name SetAccessControlTenant */
select set_config('jwt.claims.tenant_id', :tenantId!, true);

/* @name AccessControlCredentials */
select c.kind, c.code, c.person_id::text as "personId!", p.name as "personName"
from access_credential c
join person p on p.id = c.person_id
where c.tenant_id = :tenantId! and c.valid_range @> now()
  and c.person_id in (
    select person_id from app_private.current_tenant_relation
  )
order by c.id;

/* @name InsertAccessControlEvents */
with inserted as (
  insert into access_event (
    external_id, device, location_id, kind, code, person_id, occurred_at, allowed, reason
  )
  select
    event.id,
    event.device,
    :locationId!::bigint,
    event.kind::access_credential_kind,
    event.code,
    (
      select credential.person_id
      from access_credential credential
      where credential.kind = event.kind::access_credential_kind
        and credential.code = event.code
        and credential.valid_range @> event."occurredAt"
    ),
    event."occurredAt",
    event.allowed,
    event.reason
  from jsonb_to_recordset(:events!::text::jsonb) as event(
    id text,
    device text,
    kind text,
    code text,
    "occurredAt" timestamptz,
    allowed boolean,
    reason text
  )
  on conflict (tenant_id, external_id) do nothing
  returning id
)
select count(*)::int as "accepted!" from inserted;
