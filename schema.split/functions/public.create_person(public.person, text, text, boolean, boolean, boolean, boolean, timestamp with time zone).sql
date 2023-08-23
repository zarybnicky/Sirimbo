CREATE FUNCTION public.create_person(INOUT p public.person, primary_email text, primary_phone text, is_member boolean, is_trainer boolean, is_admin boolean, send_invitation boolean, join_date timestamp with time zone) RETURNS public.person
    LANGUAGE plpgsql
    AS $$
begin
  insert into person overriding user value select p.* returning * into p;
  if is_member = true then
    insert into tenant_membership (person_id, tenant_id, since, active) values (p.id, current_tenant_id(), join_date, true);
  end if;
  if is_trainer = true then
    insert into tenant_trainer (person_id, tenant_id, since, active) values (p.id, current_tenant_id(), join_date, true);
  end if;
  if is_admin = true then
    insert into tenant_administrator (person_id, tenant_id, since, active) values (p.id, current_tenant_id(), join_date, true);
  end if;
  if primary_phone is not null then
    insert into person_phone (person_id, phone, is_primary) values (p.id, primary_phone, true);
  end if;
  if primary_email is not null then
    insert into person_email (person_id, email, is_primary) values (p.id, primary_email, true);
  end if;

  if send_invitation = true then
    insert into person_invitation (person_id, tenant_id) values (p.id, current_tenant_id());
  end if;
end
$$;

GRANT ALL ON FUNCTION public.create_person(INOUT p public.person, primary_email text, primary_phone text, is_member boolean, is_trainer boolean, is_admin boolean, send_invitation boolean, join_date timestamp with time zone) TO anonymous;


