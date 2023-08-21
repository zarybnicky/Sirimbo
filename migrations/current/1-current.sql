-- Write your migration here

comment on table person is E'@omit create,delete';
comment on table platby_item is E'@omit create,update,delete';
comment on table platby_category is E'@omit create,update,delete';
comment on table platby_category_group is E'@omit create,update,delete';
comment on table platby_group is E'@omit create,update,delete';
comment on table platby_group_skupina is E'@omit create,update,delete';
comment on table platby_raw is E'@omit create,update,delete';

COMMENT ON TABLE public.tenant_location IS '@omit create,update,delete
@simpleCollections only';

select app_private.drop_policies('public.person');
create policy admin_all on person to administrator using (true);
create policy admin_myself on person for update using (id in (select my_person_ids()));
create policy view_same_tenant on person for select using (exists (select 1 from tenant_membership where active=true and person_id = id and tenant_id in (select my_tenant_ids())));
create policy view_tenant_admin on person for select using (exists (select 1 from tenant_administrator where active=true and person_id = id));
create policy view_tenant_trainer on person for select using (exists (select 1 from tenant_trainer where active=true and person_id = id));

CREATE or replace FUNCTION public.login(login character varying, passwd character varying, OUT sess public.session, OUT usr public.users) RETURNS record
    LANGUAGE plpgsql STRICT SECURITY DEFINER
    AS $$
declare
  v_salt varchar;
begin
  if login like '%@%' then
    select users.* into usr from users where lower(u_email) = lower(login) limit 1;
  else
    select users.* into usr from users where lower(u_login) = lower(login) limit 1;
  end if;

  if usr is null then
    raise exception 'ACCOUNT_NOT_FOUND' using errcode = '28000';
  end if;

  select encode(digest('######TK.-.OLYMP######', 'md5'), 'hex') into v_salt;
  if usr.u_pass != encode(digest(v_salt || passwd || v_salt, 'sha1'), 'hex') then
    raise exception 'INVALID_PASSWORD' using errcode = '28P01';
  end if;

  perform set_config('jwt.claims.user_id', usr.u_id::text, true);
  insert into session (ss_id, ss_user, ss_lifetime) values (gen_random_uuid(), usr.u_id, 86400)
  returning * into sess;
end;
$$;

GRANT ALL ON FUNCTION person_is_trainer TO anonymous;

do $$ begin
  if not exists (select 1 from information_schema.columns where table_name = 'tenant_location' and column_name = 'id') then
    ALTER TABLE tenant_location DROP CONSTRAINT tenant_location_pkey;
    ALTER TABLE tenant_location ADD COLUMN id bigint GENERATED ALWAYS AS IDENTITY PRIMARY KEY;
  end if;
end $$;

create index if not exists tenant_location_tenant_id_idx on tenant_location (tenant_id);
