/* @name HandoffEvent */
select tenant_id from event where id = :eventId! and is_public;

/* @name CreateHandoffOtp */
insert into otp_token (user_id, tenant_id, intent, event_id, expires_at)
values (:userId!, :tenantId!, :intent!, :eventId, now() + interval '10 minutes')
returning access_token;

/* @name OtpDestination */
select intent, event_id, tenant_id from otp_token where access_token = :token!;

/* @name Tenants */
select id, name, origins, cz_ico, cz_dic, to_json(address) as address,
  description, bank_account, csts_id, created_at, accent_light, accent_dark
from public.tenant order by id;
