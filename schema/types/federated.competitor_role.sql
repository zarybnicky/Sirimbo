CREATE TYPE federated.competitor_role AS ENUM (
    'lead',
    'follow',
    'member',
    'substitute'
);

COMMENT ON TYPE federated.competitor_role IS '@name competitor_role';
