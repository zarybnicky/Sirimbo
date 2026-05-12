CREATE TYPE federated.person_license_status AS ENUM (
    'active',
    'expired',
    'revoked',
    'resting',
    'retired',
    'aspiring',
    'suspended',
    'unknown'
);
