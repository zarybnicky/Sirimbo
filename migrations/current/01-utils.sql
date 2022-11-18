create schema if not exists app_private;
grant all on schema app_private to postgres;
grant all on schema app_private to :DATABASE_OWNER;

create or replace function app_private.drop_policies(tbl text) returns void as $$
declare
   rec record;
begin
   for rec in (
     select policyname from pg_policies
     where schemaname = split_part(tbl, '.', 1) and tablename = split_part(tbl, '.', 2)
   ) loop
     execute 'drop policy "' || rec.policyname || '" on ' || tbl;
   end loop;
end;
$$ language plpgsql volatile;
select * from plpgsql_check_function('app_private.drop_policies');

create or replace function app_private.tg__timestamps() returns trigger as $$
begin
  NEW.created_at = (case when TG_OP = 'INSERT' then NOW() else OLD.created_at end);
  NEW.updated_at = (case when TG_OP = 'UPDATE' and OLD.updated_at >= NOW() then OLD.updated_at + interval '1 millisecond' else NOW() end);
  return NEW;
end;
$$ language plpgsql volatile;
comment on function app_private.tg__timestamps() is
  E'This trigger should be called on all tables with created_at, updated_at - it ensures that they cannot be manipulated and that updated_at will always be larger than the previous updated_at.';


create or replace function current_session_id() returns text as $$
  select nullif(current_setting('jwt.claims.user_id', true), '')::integer;
$$ language sql stable;
grant execute on function current_session_id to anonymous;

create or replace function current_user_id() returns bigint as $$
  SELECT current_setting('jwt.claims.user_id', true)::bigint;
$$ language sql stable;
grant execute on function current_user_id to anonymous;

create or replace function current_permissions() returns setof permissions as $$
  SELECT permissions.* from permissions
  inner join users on u_group=pe_id
  where u_id=current_setting('jwt.claims.user_id', true)::bigint;
$$ language sql stable;
grant execute on function current_user_id to anonymous;

create or replace function current_couple_ids() returns setof bigint AS $$
  select distinct p_id
  from public.pary
  where p_id_partner = current_user_id() and p_archiv = false
  UNION
  select distinct p_id
  from public.pary
  where p_id_partnerka = current_user_id() and p_archiv = false;
$$ language sql stable;
grant execute on function current_couple_ids to anonymous;
