do $$ begin
  if not exists (select 1 from information_schema.tables
    where table_schema = 'public' and table_name = 'page'
  ) then

    create table page (
      id serial primary key,
      url varchar not null unique,
      content json not null,
      created_at timestamptz not null default now(),
      updated_at timestamptz not null default now()
    );
    comment on table page is E'@omit delete';

    CREATE TEMPORARY TABLE IF NOT EXISTS base_revision (
        rev_number INTEGER NOT NULL,
        rev_operation CHAR(1) NOT NULL CHECK (rev_operation IN ('I', 'U', 'D')),
        rev_timestamp TIMESTAMP DEFAULT CURRENT_TIMESTAMP
    );
    create table page_revision (
        like base_revision including constraints,
        like page,
        primary key (rev_number, id)
    );
    comment on table page_revision is E'@omit create,update,delete';

    -- ************** page **************
    perform app_private.drop_policies('public.page');
    alter table page enable row level security;
    create policy admin_all on page to administrator using (true) with check (true);
    create policy all_view on page for select using (true);
    grant usage, select on sequence page_id_seq to administrator;
    grant all on page to anonymous;

    -- ************** page_revision **************
    perform app_private.drop_policies('public.page_revision');
    alter table page_revision enable row level security;
    create policy all_view on page_revision for select using (true);
    grant all on page_revision to anonymous;
  end if;
end $$;

do $$ begin
  if not exists (select 1
     from information_schema.columns
     where table_schema = 'public' and table_name = 'page' and column_name = 'title'
  ) then
    alter table public.page add column title text not null default '';
  end if;
end $$;

do $$ begin
  if not exists (select 1
     from information_schema.columns
     where table_schema = 'public' and table_name = 'page_revision' and column_name = 'title'
  ) then
    alter table public.page_revision add column title text not null default '';
  end if;
end $$;


-- https://dev.to/livioribeiro/use-your-database-part-3---creating-a-revision-system-20j7
CREATE TEMPORARY TABLE IF NOT EXISTS base_revision (
    rev_number INTEGER NOT NULL,
    rev_operation CHAR(1) NOT NULL CHECK (rev_operation IN ('I', 'U', 'D')),
    rev_timestamp TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);

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
select * from plpgsql_check_function('app_private.insert_revision', 'page');


drop trigger if exists _100_timestamps on public.page;
create trigger _100_timestamps
  before insert or update on page
  for each row
  execute procedure app_private.tg__timestamps();

drop trigger if exists _100_page_revision on public.page;
create trigger _100_page_revision
  after insert or update or delete on page
  for each row
  execute procedure app_private.insert_revision();
