drop function if exists create_person;

create or replace function create_person(
  person_id bigint,
  inout p person,
  is_member boolean,
  is_trainer boolean,
  is_admin boolean,
  send_invitation boolean,
  join_date timestamptz
)
  returns person
  language plpgsql security definer
  set search_path = pg_catalog, public, pg_temp
as $$
begin
  if not pg_has_role(current_setting('role'), 'system_admin', 'member') then
    if not current_tenant_id() = any (current_admin_tenant_ids())
      or (person_id is not null and not app_private.can_edit_person(person_id)) then
      raise insufficient_privilege using message = 'Cannot add this person to this club';
    end if;
    if person_id is null and (is_member or is_trainer or is_admin) is not true then
      raise insufficient_privilege using message = 'A new person must have a club relationship';
    end if;
  end if;
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
      phone,
      external_ids,
      instagram_username,
      tiktok_username,
      facebook_url,
      website_url
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
      p.phone,
      p.external_ids,
      p.instagram_username,
      p.tiktok_username,
      p.facebook_url,
      p.website_url
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

grant all on function create_person to anonymous;
