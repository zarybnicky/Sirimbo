CREATE TYPE public.attendance_type AS ENUM (
    'unknown',
    'attended',
    'not-excused'
);

SET default_tablespace = '';

SET default_table_access_method = heap;
