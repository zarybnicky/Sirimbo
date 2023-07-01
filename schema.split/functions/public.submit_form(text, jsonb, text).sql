CREATE FUNCTION public.submit_form(type text, data jsonb, url text) RETURNS void
    LANGUAGE plpgsql STRICT SECURITY DEFINER
    AS $$
begin
  insert into form_responses (type, data, url) values (type, data, url);
end;
$$;

GRANT ALL ON FUNCTION public.submit_form(type text, data jsonb, url text) TO anonymous;


