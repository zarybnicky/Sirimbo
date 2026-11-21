CREATE TYPE federated.event_status AS ENUM (
    'unknown',
    'planned',
    'in_progress',
    'completed',
    'postponed',
    'cancelled'
);

COMMENT ON TYPE federated.event_status IS '@name federation_event_status';

GRANT ALL ON TYPE federated.event_status TO anonymous;
