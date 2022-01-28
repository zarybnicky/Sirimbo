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
  SELECT DISTINCT ON (u_id, s_name, u_email) u_email, s_name FROM platby_item
    INNER JOIN platby_category ON pc_id=pi_id_category
    INNER JOIN platby_category_group ON pcg_id_category=pc_id
    INNER JOIN platby_group ON pg_id=pcg_id_group
    INNER JOIN platby_group_skupina ON pgs_id_group=pg_id
    INNER JOIN skupiny ON pgs_id_skupina=s_id
    INNER JOIN users ON pi_id_user=u_id AND u_skupina=s_id
  WHERE pg_type='1'
    AND CURRENT_DATE >= pc_valid_from
    AND CURRENT_DATE <= pc_valid_to
    AND u_confirmed='1'
    AND u_ban='0'
    AND u_system='0' ORDER BY s_name, u_email;
