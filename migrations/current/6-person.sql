
CREATE or replace FUNCTION public.person_couples(p person) RETURNS SETOF public.couple LANGUAGE sql STABLE AS $$
  select couple.* from couple where man_id = p.id or woman_id = p.id;
$$;
GRANT ALL ON FUNCTION public.person_couples(person) TO anonymous;
comment on function person_couples is E'@simpleCollections only';

CREATE or replace FUNCTION public.person_has_user(p person) RETURNS boolean LANGUAGE sql STABLE AS $$
  select exists (select 1 from user_proxy where person_id = p.id);
$$;
GRANT ALL ON FUNCTION public.person_has_user(person) TO anonymous;
