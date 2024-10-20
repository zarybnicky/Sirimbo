CREATE FUNCTION public.attachment_directories() RETURNS SETOF text
    LANGUAGE sql STABLE
    AS $_$
  SELECT distinct regexp_replace(object_name, '/[^/]*$', '') from attachment;
$_$;

COMMENT ON FUNCTION public.attachment_directories() IS '@sortable';

GRANT ALL ON FUNCTION public.attachment_directories() TO anonymous;
