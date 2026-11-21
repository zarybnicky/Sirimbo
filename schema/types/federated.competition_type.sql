CREATE TYPE federated.competition_type AS ENUM (
    'cup',
    'ranking',
    'league',
    'championship',
    'top_level',
    'super_league',
    'g_cup',
    'unknown'
);

COMMENT ON TYPE federated.competition_type IS '@name competition_type';
