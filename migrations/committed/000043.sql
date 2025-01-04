--! Previous: sha1:838b327aa46c6c4716b50750b40519024850ecfb
--! Hash: sha1:b5837979801984a5667a5b5dd3be0072ea455eb0

--! split: 1-current.sql
CREATE OR REPLACE FUNCTION immutable_concat_ws(text, VARIADIC text[])
  RETURNS text
  LANGUAGE internal IMMUTABLE PARALLEL SAFE AS
  'text_concat_ws';

alter table person drop column if exists name;
alter table person add column if not exists name text not null generated always as (
  immutable_concat_ws(' ',
    nullif(trim(prefix_title), ''),
    nullif(trim(first_name), ''),
    nullif(trim(last_name), ''),
    case when suffix_title is null or trim(suffix_title) = '' then null else immutable_concat_ws(' ', ',', trim(suffix_title)) end
  )
) stored;

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
