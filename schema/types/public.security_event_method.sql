CREATE TYPE public.security_event_method AS ENUM (
    'password',
    'otp',
    'manual',
    'scheduled'
);
