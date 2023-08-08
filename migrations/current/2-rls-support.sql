
CREATE or replace FUNCTION my_person_ids() RETURNS SETOF bigint LANGUAGE sql STABLE AS $$
  select person.id
  from person join user_proxy on person_id=person.id
  where user_id = current_user_id();
$$;
GRANT ALL ON FUNCTION my_person_ids() TO anonymous;
comment on function my_person_ids is '@omit';

CREATE or replace FUNCTION my_couple_ids() RETURNS SETOF bigint LANGUAGE sql STABLE AS $$
  select couple.id
  from couple join person on (man_id = person.id or woman_id = person.id) join user_proxy on person_id=person.id
  where user_id = current_user_id();
$$;
GRANT ALL ON FUNCTION my_couple_ids() TO anonymous;
comment on function my_couple_ids is '@omit';

CREATE or replace FUNCTION my_tenant_ids() RETURNS SETOF bigint LANGUAGE sql STABLE AS $$
  select tenant.id
  from tenant join tenant_membership on tenant.id = tenant_id
  where tenant_membership.active = true and person_id in (select my_person_ids());
$$;
GRANT ALL ON FUNCTION my_tenant_ids() TO anonymous;
comment on function my_tenant_ids is '@omit';

create or replace FUNCTION current_couple_ids() RETURNS SETOF bigint LANGUAGE sql STABLE AS $$
  select my_couple_ids();
$$;
GRANT ALL ON FUNCTION public.current_couple_ids() TO anonymous;
