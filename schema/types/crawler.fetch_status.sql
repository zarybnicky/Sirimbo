CREATE TYPE crawler.fetch_status AS ENUM (
    'pending',
    'ok',
    'gone',
    'error',
    'transient'
);
