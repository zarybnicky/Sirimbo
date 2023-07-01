CREATE FUNCTION public.confirm_user(id bigint, grp bigint, cohort bigint) RETURNS void
    LANGUAGE plpgsql STRICT SECURITY DEFINER
    AS $$
declare
  usr users;
begin
  select * into usr from users where u_id=id;
  if usr is null then
    raise exception 'ACCOUNT_NOT_FOUND' using errcode = '28000';
  end if;
  update users set u_confirmed=true, u_group=grp, u_skupina=cohort, u_system=false where u_id=id;
  perform graphile_worker.add_job('notify_confirmed_user', json_build_object('id', id));
end;
$$;

GRANT ALL ON FUNCTION public.confirm_user(id bigint, grp bigint, cohort bigint) TO administrator;


