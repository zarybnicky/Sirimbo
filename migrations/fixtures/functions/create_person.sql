drop function if exists create_person;

CREATE or replace FUNCTION public.create_person(person_id bigint, INOUT p public.person, is_member boolean, is_trainer boolean, is_admin boolean, send_invitation boolean, join_date timestamp with time zone) RETURNS public.person
    LANGUAGE plpgsql
    AS $$
begin
  if person_id is null then
    insert into person (
      first_name,
      last_name,
      gender,
      birth_date,
      nationality,
      tax_identification_number,
      national_id_number,
      csts_id,
      wdsf_id,
      prefix_title,
      suffix_title,
      bio,
      email,
      phone
    ) values (
      p.first_name,
      p.last_name,
      p.gender,
      p.birth_date,
      p.nationality,
      p.tax_identification_number,
      p.national_id_number,
      p.csts_id,
      p.wdsf_id,
      p.prefix_title,
      p.suffix_title,
      p.bio,
      p.email,
      p.phone
    ) returning * into p;
  else
    select * into p from person where person.id=person_id;
  end if;
  if is_member = true then
    insert into tenant_membership (person_id, tenant_id, since) values (p.id, current_tenant_id(), join_date);
  end if;
  if is_trainer = true then
    insert into tenant_trainer (person_id, tenant_id, since) values (p.id, current_tenant_id(), join_date);
  end if;
  if is_admin = true then
    insert into tenant_administrator (person_id, tenant_id, since) values (p.id, current_tenant_id(), join_date);
  end if;
  if send_invitation = true and p.email is not null and p.email <> '' then
    insert into person_invitation (person_id, tenant_id, email) values (p.id, current_tenant_id(), p.email);
  end if;
end
$$;
select verify_function('create_person');
grant all on function create_person to anonymous;
