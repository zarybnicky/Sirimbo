CREATE TYPE federated.person_license_kind AS ENUM (
    'athlete',
    'trainer',
    'adjudicator',
    'chairperson',
    'invigilator',
    'scrutineer',
    'lead_scrutineer',
    'examiner',
    'dj',
    'head_judge',
    'official'
);

COMMENT ON TYPE federated.person_license_kind IS '@name person_license_kind';
