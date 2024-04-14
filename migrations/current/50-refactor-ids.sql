alter table aktuality
  alter column id set not null,
  drop constraint if exists aktuality_unique_id,
  add constraint aktuality_unique_id unique (id);
alter table dokumenty
  alter column id set not null,
  drop constraint if exists dokumenty_unique_id,
  add constraint dokumenty_unique_id unique (id);
alter table galerie_dir
  alter column id set not null,
  drop constraint if exists galerie_dir_unique_id,
  add constraint galerie_dir_unique_id unique (id);
alter table galerie_foto
  alter column id set not null,
  drop constraint if exists galerie_foto_unique_id,
  add constraint galerie_foto_unique_id unique (id);
alter table platby_category
  alter column id set not null,
  drop constraint if exists platby_category_unique_id,
  add constraint platby_category_unique_id unique (id);
alter table platby_category_group
  alter column id set not null,
  drop constraint if exists platby_category_group_unique_id,
  add constraint platby_category_group_unique_id unique (id);
alter table platby_group
  alter column id set not null,
  drop constraint if exists platby_group_unique_id,
  add constraint platby_group_unique_id unique (id);
alter table platby_group_skupina
  alter column id set not null,
  drop constraint if exists platby_group_skupina_unique_id,
  add constraint platby_group_skupina_unique_id unique (id);
alter table platby_item
  alter column id set not null,
  drop constraint if exists platby_item_unique_id,
  add constraint platby_item_unique_id unique (id);
alter table platby_raw
  alter column id set not null,
  drop constraint if exists platby_raw_unique_id,
  add constraint platby_raw_unique_id unique (id);
alter table upozorneni_skupiny
  alter column id set not null,
  drop constraint if exists upozorneni_skupiny_unique_id,
  add constraint upozorneni_skupiny_unique_id unique (id);
alter table users
  alter column id set not null,
  drop constraint if exists users_unique_id,
  add constraint users_unique_id unique (id);
alter table upozorneni
  alter column id set not null,
  drop constraint if exists upozorneni_unique_id,
  add constraint upozorneni_unique_id unique (id);
alter table skupiny
  alter column id set not null,
  drop constraint if exists skupiny_unique_id,
  add constraint skupiny_unique_id unique (id);
alter table app_private.pary_navrh
  alter column id set not null,
  drop constraint if exists pary_navrh_unique_id,
  add constraint pary_navrh_unique_id unique (id);
