/* @name HandoffEvent */
select tenant_id from event where id = :eventId! and is_public;

/* @name CreateHandoffOtp */
insert into otp_token (user_id, tenant_id, intent, event_id, expires_at)
values (:userId!, :tenantId!, 'event_handoff', :eventId!, now() + interval '10 minutes')
returning access_token;
