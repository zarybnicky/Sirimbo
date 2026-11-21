CREATE TYPE federated.official_role AS ENUM (
    'adjudicator',
    'chairperson',
    'invigilator',
    'scrutineer',
    'lead_scrutineer'
);

COMMENT ON TYPE federated.official_role IS '@name official_role';
