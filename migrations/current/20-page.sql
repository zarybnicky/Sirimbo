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

    create trigger _100_timestamps
      before insert or update on page
      for each row
      execute procedure app_private.tg__timestamps();

    create trigger _100_page_revision
      after insert or update or delete on page
      for each row
      execute procedure app_private.insert_revision();
  end if;
end $$;
