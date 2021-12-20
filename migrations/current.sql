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


drop table if exists page cascade;
drop table if exists page_revision cascade;

create table page (
       id serial primary key,
       url varchar not null unique,
       current_revision integer not null,
       created_at timestamptz not null default now(),
       updated_at timestamptz not null default now()
);
comment on table page is E'@omit create,update,delete';

create table page_revision (
       id serial primary key,
       page_id integer,
       content json not null,
       created_at timestamptz not null default now(),
       updated_at timestamptz not null default now(),
       foreign key (page_id) references page on delete cascade
);
comment on table page_revision is E'@omit create,update,delete';

alter table page
  add constraint page_page_revision_fkey
  foreign key (current_revision) references page_revision on delete restrict;

create trigger _100_timestamps
  before insert or update on page_revision
  for each row
  execute procedure app_private.tg__timestamps();

create trigger _100_timestamps
  before insert or update on page
  for each row
  execute procedure app_private.tg__timestamps();


create or replace function create_page(url varchar, content json) returns page as $$
declare
  new_revision integer;
  new_page page;
begin
  insert into page_revision (page_id, content) values (null, content) returning id into new_revision;
  insert into page (url, current_revision) values (url, new_revision) returning * into new_page;
  return new_page;
end;
$$ language plpgsql strict volatile;


create or replace function update_page(page_id integer, url varchar, content json) returns page as $$
declare
  new_revision integer;
  new_page page;
begin
  insert into page_revision (page_id, content) values (page_id, content) returning id into new_revision;
  update page set url=url, current_revision=new_revision where id=page_id returning * into new_page;
  return new_page;
end;
$$ language plpgsql strict volatile;
