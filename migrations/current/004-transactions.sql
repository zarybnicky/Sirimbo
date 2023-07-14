
alter table platby_item drop column if exists status;
drop type if exists payment_status;
create type payment_status as enum (
  'tentative',
  'unpaid',
  'paid'
);
alter table platby_item add column if not exists status payment_status not null default 'paid';


-- platby_item: add variable symbol
-- platby_item: transaction id
-- drop payment_group
-- make payment_category optional???
