CREATE FUNCTION public.logout() RETURNS void
    LANGUAGE plpgsql STRICT SECURITY DEFINER
    AS $$
begin
  delete from session where ss_id=current_session_id();
end;
$$;

GRANT ALL ON FUNCTION public.logout() TO anonymous;


