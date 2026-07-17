--! Previous: sha1:76e92504aa95da350d8f8b48bcd04f7565996437
--! Hash: sha1:d48f1a340f0c54b9c16199637b24feed118965da

--! split: 1-current.sql
--! Included functions/event_instance_registration_last_attended.sql
create or replace function event_instance_registration_last_attended(reg event_instance_registration)
  returns timestamp with time zone
  language sql stable as $$
  select max(attended_instance.since)
  from event_instance_registration r
  join event_instance attended_instance on attended_instance.id = r.instance_id
  join event_instance current_instance on current_instance.id = reg.instance_id
  where current_instance.series_id is not null
    and attended_instance.tenant_id = current_instance.tenant_id
    and attended_instance.series_id = current_instance.series_id
    and r.person_id = reg.person_id
    and r.status = 'attended'
$$;

grant all on function event_instance_registration_last_attended(event_instance_registration) to anonymous;
--! EndIncluded functions/event_instance_registration_last_attended.sql
--! Included functions/event_instance_approx_price.sql
create or replace function public.event_instance_approx_price(v_instance event_instance)
  returns table (amount numeric(19,4), currency text)
  language sql stable
as $$
  with stats as (
    select
      (select count(distinct registration.person_id)
       from public.event_instance_registration registration
       where registration.instance_id = v_instance.id
         and registration.person_id is not null
         and registration.registration_status = 'active')::bigint as num_participants,
      extract(epoch from (v_instance.until - v_instance.since)) / 60.0 as duration
  )
  select
    sum(tt.member_price_45min_amount * s.duration / 45 / s.num_participants) as amount,
    tt.currency as currency
  from stats s
  join lateral public.event_instance_trainers(v_instance) tt on true
  where
    s.num_participants > 0
    and s.duration > 0
    and tt.member_price_45min_amount is not null
    and tt.currency is not null
  group by tt.currency;
$$;

grant all on function event_instance_approx_price to anonymous;
COMMENT ON FUNCTION event_instance_approx_price IS '@simpleCollections only';
--! EndIncluded functions/event_instance_approx_price.sql
--! Included functions/index_advisor.sql
create or replace function app_private.index_advisor(query text) returns table (
  startup_cost_before jsonb,
  startup_cost_after jsonb,
  total_cost_before jsonb,
  total_cost_after jsonb,
  index_statements text[],
  errors text[]
)
volatile
language plpgsql
as $$
declare
  n_args int;
  prepared_statement_name text = 'index_advisor_working_statement';
  hypopg_schema_name text = (select extnamespace::regnamespace::text from pg_extension where extname = 'hypopg');
  explain_plan_statement text;
  error_message text;
  rec record;
  plan_initial jsonb;
  plan_final jsonb;
  statements text[] := array[]::text[];
  v_context text;
begin
  -- Remove comment lines (it is common for them to contain semicolons).
  query := trim(
    regexp_replace(
      regexp_replace(
        regexp_replace(query, '\/\*.+\*\/', '', 'g'),
        '--[^\r\n]*',
        ' ',
        'g'
      ),
      '\s+',
      ' ',
      'g'
    )
  );

  query := regexp_replace(query, ';\s*$', '');

  begin
    if query ilike '%;%' then
      raise exception 'Query must not contain a semicolon';
    end if;

    -- Hack to support PostgREST because prepared-statement arguments default to text.
    query := replace(
      query,
      'WITH pgrst_payload AS (SELECT $1 AS json_data)',
      'WITH pgrst_payload AS (SELECT $1::json AS json_data)'
    );

    deallocate all;
    perform plpgsql_check_pragma('disable:security_warnings');
    execute format('prepare %I as %s', prepared_statement_name, query);
    perform plpgsql_check_pragma('enable:security_warnings');

    n_args := (
      select coalesce(array_length(parameter_types, 1), 0)
      from pg_prepared_statements
      where name = prepared_statement_name
      limit 1
    );

    explain_plan_statement := format(
      'set local plan_cache_mode = force_generic_plan; explain (format json) execute %I%s',
      prepared_statement_name,
      case
        when n_args = 0 then ''
        else format(
          '(%s)',
          array_to_string(array_fill('null'::text, array[n_args]), ',')
        )
      end
    );
    execute explain_plan_statement into plan_initial;

    for rec in (
      with extension_regclass as (
        select distinct objid as oid
        from pg_catalog.pg_depend
        where deptype = 'e'
      )
      select
        pc.relnamespace::regnamespace::text as schema_name,
        pc.relname as table_name,
        pa.attname as column_name,
        format(
          'select %I.hypopg_create_index($i$create index on %I.%I(%I)$i$)',
          hypopg_schema_name,
          pc.relnamespace::regnamespace::text,
          pc.relname,
          pa.attname
        ) as hypopg_statement
      from pg_catalog.pg_class pc
      join pg_catalog.pg_attribute pa on pc.oid = pa.attrelid
      left join extension_regclass er on pc.oid = er.oid
      left join pg_catalog.pg_index pi
        on pc.oid = pi.indrelid
       and (select array_agg(x) from unnest(pi.indkey) v(x)) = array[pa.attnum]
       and pi.indexprs is null
       and pi.indpred is null
      where pc.relnamespace::regnamespace::text not in (
        'pg_catalog',
        'pg_toast',
        'information_schema'
      )
        and er.oid is null
        and pc.relkind in ('r', 'm')
        and pc.relpersistence = 'p'
        and pa.attnum > 0
        and not pa.attisdropped
        and pi.indrelid is null
        and pa.atttypid in (
          20,
          16,
          1082,
          1184,
          1114,
          701,
          23,
          21,
          700,
          1083,
          2950,
          1700,
          25,
          18,
          1042,
          1043
        )
    ) loop
      perform plpgsql_check_pragma('disable:security_warnings');
      execute rec.hypopg_statement;
      perform plpgsql_check_pragma('enable:security_warnings');
    end loop;

    deallocate index_advisor_working_statement;
    perform plpgsql_check_pragma('disable:security_warnings');
    execute format('prepare %I as %s', prepared_statement_name, query);
    perform plpgsql_check_pragma('enable:security_warnings');

    execute explain_plan_statement into plan_final;

    execute format(
      'select
         coalesce(
           array_agg(hypopg_get_indexdef(indexrelid) order by indrelid, indkey::text),
           $i${}$i$::text[]
         )
       from %I.hypopg()
       where %s ilike ($i$%%$i$ || indexname || $i$%%$i$)',
      hypopg_schema_name,
      quote_literal(plan_final)::text
    ) into statements;

    perform hypopg_reset();
    deallocate all;

    return query values (
      plan_initial -> 0 -> 'Plan' -> 'Startup Cost',
      plan_final -> 0 -> 'Plan' -> 'Startup Cost',
      plan_initial -> 0 -> 'Plan' -> 'Total Cost',
      plan_final -> 0 -> 'Plan' -> 'Total Cost',
      statements,
      array[]::text[]
    );
    return;
  exception when others then
    get stacked diagnostics
      error_message = message_text,
      v_context = pg_exception_context;

    return query values (
      null::jsonb,
      null::jsonb,
      null::jsonb,
      null::jsonb,
      array[]::text[],
      array[error_message, v_context]::text[]
    );
    return;
  end;
end;
$$;

comment on function app_private.index_advisor(text) is '@omit';
--! EndIncluded functions/index_advisor.sql
--! Included functions/login.sql
CREATE or replace FUNCTION login(login text, passwd text) RETURNS login_result
  LANGUAGE plpgsql SECURITY DEFINER
  SET search_path TO pg_catalog, public, pg_temp
AS $$
declare
  v_salt text;
  usr users;
  jwt jwt_token;
begin
  v_salt := encode(digest('######TK.-.OLYMP######', 'md5'), 'hex');

  select u.* into usr
  from users u
  where (lower(u.u_login) = lower(trim(login)) or lower(u.u_email) = lower(trim(login)))
    and u.u_pass = encode(digest(v_salt || passwd || v_salt, 'sha1'), 'hex');

  if usr is null then
    raise exception 'INVALID_CREDENTIALS' using errcode = '28P01';
  end if;

  jwt := app_private.create_jwt_token(usr);
  perform set_config('jwt.claims.user_id', jwt.user_id::text, true);
  perform set_config('jwt.claims.my_person_ids', jwt.my_person_ids::text, true);
  perform set_config('jwt.claims.my_tenant_ids', jwt.my_tenant_ids::text, true);
  perform set_config('jwt.claims.my_cohort_ids', jwt.my_cohort_ids::text, true);
  perform set_config('jwt.claims.my_couple_ids', jwt.my_couple_ids::text, true);
  update users set last_login = now() where id = usr.id;
  return (usr, jwt);
end;
$$;

select verify_function('login');
GRANT ALL ON FUNCTION login TO anonymous;
--! EndIncluded functions/login.sql
--! Included functions/register_using_invitation.sql
CREATE or replace FUNCTION public.register_using_invitation(email text, passwd text, token uuid, login text default null)
  RETURNS login_result
  LANGUAGE plpgsql SECURITY DEFINER
  SET search_path TO pg_catalog, public, pg_temp
  AS $$
declare
  invitation person_invitation;
  v_salt text;
  usr users;
  jwt jwt_token;
begin
  select * into invitation from person_invitation where access_token=token;

  if invitation is null then
    raise exception 'INVITATION_NOT_FOUND' using errcode = '28000';
  end if;
  if invitation.used_at is not null then
    raise exception 'INVITATION_ALREADY_USED' using errcode = '28P01';
  end if;
  if email is null or email = '' then
    raise exception 'INVALID_EMAIL' using errcode = '28P01';
  end if;

  v_salt := encode(digest('######TK.-.OLYMP######', 'md5'), 'hex');
  insert into users (u_login, u_email, u_pass) values (trim(login), email, encode(digest(v_salt || passwd || v_salt, 'sha1'), 'hex')) returning * into usr;
  insert into user_proxy (user_id, person_id) values (usr.id, invitation.person_id);
  update person_invitation set used_at=now() where access_token=token;
  jwt := app_private.create_jwt_token(usr);
  perform set_config('jwt.claims.user_id', jwt.user_id::text, true);
  perform set_config('jwt.claims.my_person_ids', jwt.my_person_ids::text, true);
  perform set_config('jwt.claims.my_tenant_ids', jwt.my_tenant_ids::text, true);
  perform set_config('jwt.claims.my_cohort_ids', jwt.my_cohort_ids::text, true);
  perform set_config('jwt.claims.my_couple_ids', jwt.my_couple_ids::text, true);
  return (usr, jwt);
end
$$;

select verify_function('register_using_invitation');
GRANT ALL ON FUNCTION public.register_using_invitation TO anonymous;
--! EndIncluded functions/register_using_invitation.sql
--! Included functions/register_without_invitation.sql
CREATE or replace FUNCTION register_without_invitation(email text, passwd text) RETURNS login_result
    LANGUAGE plpgsql STRICT SECURITY DEFINER
    SET search_path TO pg_catalog, public, pg_temp
    AS $$
declare
  v_salt text;
  usr users;
  jwt jwt_token;
begin
  v_salt := encode(digest('######TK.-.OLYMP######', 'md5'), 'hex');
  insert into users (u_email, u_pass) values (email, encode(digest(v_salt || passwd || v_salt, 'sha1'), 'hex')) returning * into usr;
  jwt := app_private.create_jwt_token(usr);
  perform set_config('jwt.claims.user_id', jwt.user_id::text, true);
  perform set_config('jwt.claims.my_person_ids', jwt.my_person_ids::text, true);
  perform set_config('jwt.claims.my_tenant_ids', jwt.my_tenant_ids::text, true);
  perform set_config('jwt.claims.my_cohort_ids', jwt.my_cohort_ids::text, true);
  perform set_config('jwt.claims.my_couple_ids', jwt.my_couple_ids::text, true);
  return (usr, jwt);
end
$$;

GRANT ALL ON FUNCTION register_without_invitation TO anonymous;
--! EndIncluded functions/register_without_invitation.sql
--! Included functions/set_event_instance_registration.sql
drop function if exists public.set_event_instance_registration(bigint, bigint, bigint, boolean);
drop function if exists public.set_event_instance_registration(bigint, bigint, bigint, boolean, text);

create or replace function public.set_event_instance_registration(
  p_instance_id bigint,
  p_person_id bigint,
  p_couple_id bigint,
  p_is_registered boolean,
  p_note text default null,
  p_lesson_trainer_ids bigint[] default null,
  p_lesson_counts integer[] default null
) returns public.event_instance_registration
  language plpgsql
  security definer
  set search_path = pg_catalog, public, pg_temp
as $$
declare
  target_instance event_instance;
  registration event_instance_registration;
  registration_found boolean;
  is_manager boolean;
  is_self boolean;
  registration_source event_registration_source;
  required_capacity integer;
  remaining_capacity integer;
  lesson_demand record;
begin
  if p_is_registered is null or num_nonnulls(p_person_id, p_couple_id) <> 1 then
    raise exception 'INVALID_REGISTRANT' using errcode = '22023';
  end if;

  if num_nonnulls(p_lesson_trainer_ids, p_lesson_counts) = 1
    or cardinality(p_lesson_trainer_ids) <> cardinality(p_lesson_counts)
    or exists (
      select 1
      from unnest(p_lesson_trainer_ids, p_lesson_counts) demand(trainer_id, lesson_count)
      where demand.trainer_id is null
        or demand.lesson_count is null
        or demand.lesson_count < 0
    ) then
    raise exception 'INVALID_LESSON_DEMANDS' using errcode = '22023';
  end if;
  if not p_is_registered and p_lesson_trainer_ids is not null then
    raise exception 'INVALID_LESSON_DEMANDS' using errcode = '22023';
  end if;

  select * into target_instance
  from event_instance
  where id = p_instance_id
    and tenant_id = current_tenant_id()
  for update;

  if not found then
    raise exception 'INSTANCE_NOT_FOUND' using errcode = '22023';
  end if;

  is_self := (p_person_id is not null
      and p_person_id = any(coalesce(current_person_ids(), '{}'::bigint[])))
    or (p_couple_id is not null
      and p_couple_id = any(coalesce(current_couple_ids(), '{}'::bigint[])));
  is_manager := app_private.is_system_admin(current_user_id())
    or exists (
      select 1 from current_tenant_administrator
      where person_id = any(coalesce(current_person_ids(), '{}'::bigint[]))
    )
    or (
      exists (
        select 1 from current_tenant_trainer
        where person_id = any(coalesce(current_person_ids(), '{}'::bigint[]))
      )
      and app_private.can_trainer_edit_instance(p_instance_id)
    );

  if not is_self and not is_manager then
    raise exception 'ACCESS_DENIED' using errcode = '42501';
  end if;
  if target_instance.is_locked and not is_manager then
    raise exception 'NOT_ALLOWED' using errcode = '28000';
  end if;
  registration_source := case when is_self
    then 'self'::event_registration_source
    else 'manager'::event_registration_source
  end;

  select * into registration
  from event_instance_registration
  where instance_id = p_instance_id
    and parent_registration_id is null
    and person_id is not distinct from p_person_id
    and couple_id is not distinct from p_couple_id;
  registration_found := found;

  if not p_is_registered then
    if not registration_found then
      raise exception 'REGISTRATION_NOT_FOUND' using errcode = '22023';
    end if;

    update event_instance_registration
    set registration_status = 'cancelled',
        target_cohort_id = null,
        source = case when id = registration.id
          then registration_source end
    where id = registration.id or parent_registration_id = registration.id;

    select * into registration from event_instance_registration where id = registration.id;
    return registration;
  end if;

  if registration_found and registration.registration_status = 'active' then
    if p_note is not null then
      update event_instance_registration set note = p_note where id = registration.id
      returning * into registration;
    end if;
  else
    if not is_manager and (
      target_instance.is_cancelled
        or target_instance.until <= now()
        or not (
          coalesce(target_instance.is_public, false)
          or (
            coalesce(target_instance.is_visible, false)
            and current_tenant_id() = any(my_tenants_array())
          )
        )
    ) then
      raise exception 'NOT_ALLOWED' using errcode = '28000';
    end if;

    if not is_manager then
      remaining_capacity := event_instance_remaining_person_spots(target_instance);
      required_capacity := case
        when target_instance.capacity_unit = 'people' and p_couple_id is not null then 2
        else 1
      end;
      if remaining_capacity is not null and remaining_capacity < required_capacity then
        raise exception 'CAPACITY_EXCEEDED' using errcode = '22023';
      end if;
    end if;

    if registration_found then
      update event_instance_registration
      set registration_status = 'active',
          target_cohort_id = null,
          source = case when id = registration.id
            then registration_source end
      where id = registration.id or parent_registration_id = registration.id;

      if p_note is not null then
        update event_instance_registration set note = p_note where id = registration.id;
      end if;
    else
      insert into event_instance_registration (
        instance_id, person_id, couple_id, source, status, note
      ) values (
        p_instance_id, p_person_id, p_couple_id, registration_source,
        case when p_person_id is not null then 'unknown'::attendance_type end,
        p_note
      ) returning * into registration;

      insert into event_instance_registration (
        instance_id, parent_registration_id, person_id, status
      )
      select p_instance_id, registration.id, person.person_id, 'unknown'
      from couple
      cross join lateral unnest(array[couple.man_id, couple.woman_id]) person(person_id)
      where couple.id = p_couple_id;
    end if;
  end if;

  select * into registration from event_instance_registration where id = registration.id;

  if p_lesson_trainer_ids is not null then
    delete from event_lesson_demand
    where registration_id = registration.id
      and trainer_id <> all(p_lesson_trainer_ids);

    for lesson_demand in
      select demand.trainer_id, demand.lesson_count
      from unnest(p_lesson_trainer_ids, p_lesson_counts)
        demand(trainer_id, lesson_count)
      order by demand.trainer_id
    loop
      perform set_lesson_demand(
        registration.id,
        lesson_demand.trainer_id,
        lesson_demand.lesson_count
      );
    end loop;
  end if;

  return registration;
end;
$$;

select verify_function('public.set_event_instance_registration');
grant execute on function public.set_event_instance_registration(
  bigint, bigint, bigint, boolean, text, bigint[], integer[]
)
  to anonymous;
--! EndIncluded functions/set_event_instance_registration.sql
--! Included functions/set_lesson_demand.sql
drop function if exists set_lesson_demand(bigint, bigint, integer);

CREATE FUNCTION set_lesson_demand(instance_registration_id bigint, instance_trainer_id bigint, lesson_count integer) RETURNS event_lesson_demand
  LANGUAGE plpgsql STRICT SECURITY DEFINER
  SET search_path TO pg_catalog, public, pg_temp
AS $$
declare
  registration event_instance_registration;
  instance event_instance;
  trainer event_instance_trainer;
  lesson_demand event_lesson_demand;
  is_manager boolean;
  is_self boolean;
  other_lessons bigint;
begin
  select * into registration
  from event_instance_registration
  where id = $1 and parent_registration_id is null;

  if not found then
    raise exception 'REGISTRATION_NOT_FOUND' using errcode = '28000';
  end if;

  select * into instance
  from event_instance
  where id = registration.instance_id;

  if not found then
    raise exception 'INSTANCE_NOT_FOUND' using errcode = '28000';
  end if;

  is_self := (registration.person_id is not null
      and registration.person_id = any(coalesce(current_person_ids(), '{}'::bigint[])))
    or (registration.couple_id is not null
      and registration.couple_id = any(coalesce(current_couple_ids(), '{}'::bigint[])));
  is_manager := app_private.is_system_admin(current_user_id())
    or exists (
      select 1 from current_tenant_administrator
      where person_id = any(coalesce(current_person_ids(), '{}'::bigint[]))
    )
    or (
      exists (
        select 1 from current_tenant_trainer
        where person_id = any(coalesce(current_person_ids(), '{}'::bigint[]))
      )
      and app_private.can_trainer_edit_instance(instance.id)
    );

  if not is_self and not is_manager then
    raise exception 'ACCESS_DENIED' using errcode = '42501';
  end if;
  if instance.is_locked and not is_manager then
    raise exception 'NOT_ALLOWED' using errcode = '28000';
  end if;

  select * into trainer
  from event_instance_trainer
  where id = $2 and instance_id = instance.id
  for update;

  if not found then
    raise exception 'TRAINER_NOT_FOUND' using errcode = '28000';
  end if;

  if $3 = 0 then
    delete from event_lesson_demand eld where eld.registration_id = registration.id and eld.trainer_id = $2;
    return null;
  end if;
  if trainer.lessons_offered = 0 then
    raise exception 'LESSONS_NOT_OFFERED' using errcode = '28000';
  end if;
  if trainer.lessons_offered is not null then
    select coalesce(sum(eld.lesson_count), 0) into other_lessons
    from event_lesson_demand eld
    join event_instance_registration other_registration
      on other_registration.id = eld.registration_id
     and other_registration.registration_status = 'active'
    where eld.trainer_id = $2 and eld.registration_id <> registration.id;

    if $3 > trainer.lessons_offered - other_lessons then
      raise exception 'LESSON_LIMIT_EXCEEDED' using errcode = '22023';
    end if;
  end if;

  INSERT INTO event_lesson_demand (registration_id, trainer_id, lesson_count)
  values (registration.id, $2, $3)
  on conflict on constraint eld_unique_registration_trainer_key do update set lesson_count = $3
  returning * into lesson_demand;

  return lesson_demand;
end;
$$;

select verify_function('set_lesson_demand');
GRANT ALL ON FUNCTION set_lesson_demand(bigint, bigint, integer) TO anonymous;
--! EndIncluded functions/set_lesson_demand.sql

drop function if exists federated.competition_sankey_links;

create or replace function public.current_claims() returns jsonb
    language sql stable security definer
as $$
  select to_jsonb(app_private.create_jwt_token(users))
    - 'exp'
    - 'is_member'
    - 'is_trainer'
    - 'is_admin'
  from users
  where id = nullif(current_setting('jwt.claims.user_id', true), '')::bigint;
$$;

grant all on function public.current_claims() to anonymous;

comment on function competition_brief is '@omit';
comment on function competition_report is '@omit';

alter table crawler.frontier
  add column if not exists last_response_id bigint,
  add column if not exists last_successful_response_id bigint;

create index if not exists frontier_last_response_id_idx
  on crawler.frontier (last_response_id)
  where last_response_id is not null;

create index if not exists frontier_last_successful_response_id_idx
  on crawler.frontier (last_successful_response_id)
  where last_successful_response_id is not null;

create index if not exists json_response_content_hash_idx
  on crawler.json_response (content_hash);

do $$
begin
  if not exists (select 1 from pg_constraint where conrelid = 'crawler.frontier'::regclass and conname = 'frontier_last_response_id_fkey') then
    alter table crawler.frontier
      add constraint frontier_last_response_id_fkey
      foreign key (last_response_id)
      references crawler.json_response (id)
      on delete set null;
  end if;

  if not exists (select 1 from pg_constraint where conrelid = 'crawler.frontier'::regclass and conname = 'frontier_last_successful_response_id_fkey') then
    alter table crawler.frontier
      add constraint frontier_last_successful_response_id_fkey
      foreign key (last_successful_response_id)
      references crawler.json_response (id)
      on delete set null;
  end if;
end;
$$;

with latest_response as (
  select distinct on (frontier_id) frontier_id, id
  from crawler.json_response
  order by frontier_id, fetched_at desc, id desc
), latest_successful_response as (
  select distinct on (frontier_id) frontier_id, id
  from crawler.json_response
  where error is null
    and http_status between 200 and 399
  order by frontier_id, fetched_at desc, id desc
)
update crawler.frontier f
set
  last_response_id = latest_response.id,
  last_successful_response_id = latest_successful_response.id
from latest_response
left join latest_successful_response using (frontier_id)
where latest_response.frontier_id = f.id
  and (
    f.last_response_id is distinct from latest_response.id
    or f.last_successful_response_id is distinct from latest_successful_response.id
  );

create or replace view crawler.frontier_failure as
select
  f.id,
  f.federation,
  f.kind,
  f.key,
  f.fetch_status,
  f.process_status,
  f.error_count,
  f.next_fetch_at,
  case
    when f.fetch_status in ('error', 'transient') and f.process_status = 'error'
      then greatest(
        coalesce(jr.fetched_at, f.last_fetched_at, f.discovered_at),
        coalesce(f.last_process_error_at, f.last_fetched_at, f.discovered_at)
      )
    when f.process_status = 'error'
      then coalesce(f.last_process_error_at, f.last_fetched_at, f.discovered_at)
    else coalesce(jr.fetched_at, f.last_fetched_at, f.discovered_at)
  end as failed_at,
  jr.url,
  jr.http_status,
  jr.error as response_error,
  f.last_process_error as process_error,
  concat_ws(
    '+',
    case when f.fetch_status in ('error', 'transient') then 'fetch' end,
    case when f.process_status = 'error' then 'process' end
  ) as failure,
  coalesce(
    case when f.process_status = 'error' then f.last_process_error end,
    jr.error,
    f.last_process_error,
    ''
  ) as error_text
from crawler.frontier f
left join crawler.json_response jr on jr.id = f.last_response_id
where f.fetch_status in ('error', 'transient')
   or f.process_status = 'error';

drop index if exists crawler.json_response_frontier_fetched_desc_idx;

drop table if exists crawler.html_response;
drop table if exists crawler.html_response_cache;

-- Restore the verifier after historical migrations have finished replaying.
create or replace function public.verify_function(f regproc, relid regclass default 0)
returns void
language plpgsql volatile security invoker
set search_path = pg_catalog, public, pg_temp
as $$
declare
  issues text;
begin
  select string_agg(
    concat_ws(
      E'\n',
      format(
        '%s:%s:%s:%s: %s',
        issue.level,
        issue.sqlstate,
        coalesce(issue.lineno::text, '?'),
        issue.statement,
        issue.message
      ),
      'Query: ' || issue.query,
      'Detail: ' || issue.detail,
      'Hint: ' || issue.hint,
      'Context: ' || issue.context
    ),
    E'\n\n' order by issue.lineno nulls last, issue.level, issue.message
  ) into issues
  from plpgsql_check_function_tb(
    funcoid => f,
    relid => relid,
    fatal_errors => false,
    all_warnings => true
  ) issue;

  if issues is not null then
    raise exception 'Error when checking function %', f using detail = issues;
  end if;
end;
$$;

comment on function public.verify_function(regproc, regclass) is '@omit';
revoke execute on function public.verify_function(regproc, regclass) from public;

create or replace function app_private.drop_policies(tbl text) returns void language plpgsql as $$
declare
  target regclass := tbl::regclass;
  policy_name name;
begin
  for policy_name in
    select polname
    from pg_catalog.pg_policy
    where polrelid = target
  loop
    execute format('drop policy %I on %s', policy_name, target);
  end loop;
end;
$$;

create or replace function app_private.tg_users__encrypt_password() returns trigger language plpgsql as $$
declare
  v_salt varchar;
begin
  if length(new.u_pass) <> 40 then
    v_salt := encode(digest('######TK.-.OLYMP######', 'md5'), 'hex');
    new.u_pass := encode(digest(v_salt || new.u_pass || v_salt, 'sha1'), 'hex');
  end if;
  return new;
end;
$$;

create or replace function app_private.tg_users__trim_login() returns trigger language plpgsql as $$
begin
  new.u_login := trim(new.u_login);
  return new;
end;
$$;
