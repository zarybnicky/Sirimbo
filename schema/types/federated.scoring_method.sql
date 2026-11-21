CREATE TYPE federated.scoring_method AS ENUM (
    'skating_marks',
    'skating_places',
    'ajs-3.0'
);

COMMENT ON TYPE federated.scoring_method IS '@name scoring_method';
