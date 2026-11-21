CREATE FUNCTION public.resend_person_invitation(invitation_id bigint) RETURNS void
    LANGUAGE plpgsql STRICT SECURITY DEFINER
    SET search_path TO 'pg_catalog', 'public', 'pg_temp'
    AS $$
declare
  invitation person_invitation;
begin
  select * into invitation from person_invitation
  where id = invitation_id and tenant_id = current_tenant_id()
  for update;

  if not found then
    raise exception 'INVITATION_NOT_FOUND' using errcode = 'P0002';
  end if;
  if invitation.used_at is not null then
    raise exception 'INVITATION_ALREADY_USED' using errcode = '22023';
  end if;

  perform graphile_worker.add_job('send_invitation', json_build_object('id', invitation.id));
end;
$$;

GRANT ALL ON FUNCTION public.resend_person_invitation(invitation_id bigint) TO administrator;
