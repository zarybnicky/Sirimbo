CREATE FUNCTION public.set_event_sharing(id bigint, p_enabled boolean) RETURNS text
    LANGUAGE plpgsql
    AS $_$
declare
  v_share_token text;
begin
  update event_instance instance
  set share_token = case
    when not coalesce(p_enabled, false) then null
    when instance.share_token is null
      then translate(encode(gen_random_bytes(24), 'base64'), '+/', '-_')
    else instance.share_token
  end
  where instance.id = $1
  returning instance.share_token into v_share_token;

  if not found then
    raise exception 'INSTANCE_NOT_FOUND' using errcode = '22023';
  end if;

  return v_share_token;
end;
$_$;

GRANT ALL ON FUNCTION public.set_event_sharing(id bigint, p_enabled boolean) TO anonymous;
