CREATE TYPE federated.event_status AS ENUM (
    'unknown',
    'planned',
    'in_progress',
    'completed',
    'postponed',
    'cancelled'
);
