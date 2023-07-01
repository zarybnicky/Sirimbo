CREATE FUNCTION public.attachment_directory(attachment public.attachment) RETURNS text
    LANGUAGE sql STABLE
    AS $_$
  SELECT regexp_replace(attachment.object_name, '/[^/]*$', '');
$_$;

COMMENT ON FUNCTION public.attachment_directory(attachment public.attachment) IS '@filterable';

GRANT ALL ON FUNCTION public.attachment_directory(attachment public.attachment) TO anonymous;


