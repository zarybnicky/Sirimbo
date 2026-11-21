CREATE TYPE federated.competitor_type AS ENUM (
    'couple',
    'solo',
    'duo',
    'trio',
    'formation',
    'group',
    'team'
);

COMMENT ON TYPE federated.competitor_type IS '@name competitor_type';
