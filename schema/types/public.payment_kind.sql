CREATE TYPE public.payment_kind AS ENUM (
    'event',
    'cohort-subscription',
    'other'
);
