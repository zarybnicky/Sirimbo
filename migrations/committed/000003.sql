--! Previous: sha1:a606623ac50e1c7ea1d595dabe55c80735b56453
--! Hash: sha1:d0cd177bc6b88a640138c62ee96edf775c68dabb

drop view if exists aktuality_admin;
drop view if exists rozpis_admin;
drop view if exists nabidka_admin;

drop schema if exists app_private cascade;
create schema app_private;
grant all on schema app_private to postgres;
grant all on schema app_private to :DATABASE_OWNER;


create function app_private.tg__timestamps() returns trigger as $$
begin
  NEW.created_at = (case when TG_OP = 'INSERT' then NOW() else OLD.created_at end);
  NEW.updated_at = (case when TG_OP = 'UPDATE' and OLD.updated_at >= NOW() then OLD.updated_at + interval '1 millisecond' else NOW() end);
  return NEW;
end;
$$ language plpgsql volatile;
comment on function app_private.tg__timestamps() is
  E'This trigger should be called on all tables with created_at, updated_at - it ensures that they cannot be manipulated and that updated_at will always be larger than the previous updated_at.';


-- https://dev.to/livioribeiro/use-your-database-part-3---creating-a-revision-system-20j7
CREATE TEMPORARY TABLE IF NOT EXISTS base_revision (
    rev_number INTEGER NOT NULL,
    rev_operation CHAR(1) NOT NULL CHECK (rev_operation IN ('I', 'U', 'D')),
    rev_timestamp TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);

drop function if exists insert_revision;

CREATE OR REPLACE FUNCTION app_private.insert_revision() RETURNS TRIGGER AS $$
DECLARE
    _op CHAR(1);
    _record RECORD;
    _rev_number INTEGER;
    _rev_table VARCHAR := TG_TABLE_NAME || '_revision';
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



drop table if exists page cascade;
drop table if exists page_revision cascade;

create table page (
       id serial primary key,
       url varchar not null unique,
       content json not null,
       created_at timestamptz not null default now(),
       updated_at timestamptz not null default now()
);
comment on table page is E'@omit delete';

create table page_revision (
    like base_revision including constraints,
    like page,
    primary key (rev_number, id)
);
comment on table page_revision is E'@omit create,update,delete';

create trigger _100_timestamps
  before insert or update on page
  for each row
  execute procedure app_private.tg__timestamps();

create trigger _100_page_revision
  after insert or update or delete on page
  for each row
  execute procedure app_private.insert_revision();
