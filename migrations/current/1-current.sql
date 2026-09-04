alter table file add column if not exists url text
  generated always as ('/f/' || id || '/' || name) stored not null;
