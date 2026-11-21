CREATE TYPE federated.gender AS ENUM (
    'male',
    'female',
    'other',
    'unknown'
);

COMMENT ON TYPE federated.gender IS '@name gender';
