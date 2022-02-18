drop view if exists nabidka_admin;
drop view if exists rozpis_admin;
drop view if exists aktuality_admin;

create or replace function public.title_videos() returns setof video as $$
  select * from video where v_id in (
    select pa_value::bigint from parameters where pa_name in (
      'title_video1', 'title_video2', 'title_video3', 'title_video4'
    )
  );
$$ language sql stable;

CREATE OR REPLACE FUNCTION app_private.insert_revision() RETURNS TRIGGER AS $$
DECLARE
    _op CHAR(1);
    _record RECORD;
    _rev_number INTEGER;
    _rev_table VARCHAR := TG_TABLE_SCHEMA || '.' || TG_TABLE_NAME || '_revision';
    _where VARCHAR := '';
    _pk VARCHAR;
BEGIN
    IF TG_OP = 'INSERT' THEN
        _op := 'I'; _record := NEW;
    ELSIF TG_OP = 'UPDATE' THEN
        _op := 'U'; _record := NEW;
    ELSE
        _op := 'D'; _record := OLD;
    END IF;
    IF TG_NARGS = 0 THEN
        _where := '_rev_table.id = $1.id';
    ELSE
        _where := format('_rev_table.%1$s = $1.%1$s', TG_ARGV[0]);
        FOREACH _pk IN ARRAY TG_ARGV[1:] LOOP
            _where := _where || format(' AND _rev_table.%1$s = $1.%1$s', _pk);
        END LOOP;
    END IF;
    EXECUTE format('SELECT coalesce(max(rev_number), 0) FROM %s _rev_table WHERE %s', _rev_table, _where)
        INTO _rev_number
        USING _record;
    EXECUTE format('INSERT INTO %s VALUES ($1, $2, $3, $4.*)', _rev_table)
        USING _rev_number + 1, _op, now(), _record;
    RETURN _record;
END;
$$ LANGUAGE plpgsql SECURITY DEFINER;
select plpgsql_check_function('app_private.insert_revision', 'page');

do $$ begin
  if not exists (select 1
     from information_schema.columns
     where table_schema = 'public' and table_name = 'skupiny' and column_name = 's_location'
  ) then
    alter table public.skupiny add column s_location text not null default '';
  end if;
end $$;

do $$ begin
  if not exists (select 1
     from information_schema.columns
     where table_schema = 'public' and table_name = 'skupiny' and column_name = 's_visible'
  ) then
    alter table public.skupiny add column s_visible boolean not null default true;
  end if;
end $$;

CREATE OR REPLACE VIEW public.members AS
  WITH current_payments as (
    SELECT * from platby_item
    INNER JOIN platby_category ON pi_id_category=pc_id
    INNER JOIN platby_category_group ON pcg_id_category=pc_id
    INNER JOIN platby_group ON pg_id=pcg_id_group
    WHERE pg_type='1'
    AND CURRENT_DATE >= pc_valid_from
    AND CURRENT_DATE <= pc_valid_to
  )
  SELECT u_email, s_name, (SELECT EXISTS (SELECT pi_id FROM current_payments WHERE pi_id_user=u_id)) as payment_valid
    FROM users INNER JOIN skupiny ON s_id=u_skupina
  WHERE u_confirmed='1'
    AND u_ban='0'
    AND u_system='0'
  ORDER BY s_name, u_email;


create or replace function public.login(login varchar, passwd varchar, OUT couple pary, OUT sess session, out usr users) as $$
declare
  v_salt varchar;
begin
  if login like '%@%' then
    select users.* into usr from users where u_email = login limit 1;
  else
    select users.* into usr from users where u_login = login limit 1;
  end if;

  if usr is null then
    raise exception 'ACCOUNT_NOT_FOUND' using errcode = '28000';
  end if;

  select encode(digest('######TK.-.OLYMP######', 'md5'), 'hex') into v_salt;
  if usr.u_pass != encode(digest(v_salt || passwd || v_salt, 'sha1'), 'hex') then
    raise exception 'INVALID_PASSWORD' using errcode = '28P01';
  end if;

  if usr.u_ban then
    raise exception 'ACCOUNT_DISABLED' using errcode = '42501';
  end if;
  if not usr.u_confirmed then
    raise exception 'ACCOUNT_NOT_CONFIRMED' using errcode = '42501';
  end if;

  insert into session
    (ss_id, ss_user, ss_data, ss_lifetime)
    values (gen_random_uuid(), usr.u_id, ('{"id":' || usr.u_id || '}')::bytea, 86400)
    returning * into sess;

  select * from pary where p_archiv=false and (p_id_partner=usr.u_id or p_id_partnerka=usr.u_id) into couple;
end;
$$ language plpgsql strict volatile security definer;
select plpgsql_check_function('public.login');



create or replace function public.schedules_for_range(start_date date, end_date date) returns setof rozpis as $$
  select * from rozpis
  where r_visible=true
  and r_datum >= start_date and r_datum <= end_date
  order by r_datum asc;
$$ language sql stable;

create or replace function public.reservations_for_range(start_date date, end_date date) returns setof nabidka as $$
  select * from nabidka
  where n_visible=true
  and n_od <= start_date and n_do >= end_date
  order by n_od asc;
$$ language sql stable;

