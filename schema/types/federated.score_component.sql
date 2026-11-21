CREATE TYPE federated.score_component AS ENUM (
    'mark',
    'places',
    'ajs_tq',
    'ajs_mm',
    'ajs_ps',
    'ajs_cp',
    'ajs_reduction'
);

COMMENT ON TYPE federated.score_component IS '@name score_component';
