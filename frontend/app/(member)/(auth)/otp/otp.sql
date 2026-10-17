/* @name OtpDestination */
select intent, event_id, tenant_id from otp_token where access_token = :token!;
