-- Write your migration here

do $$
begin
  if not exists (select * from pg_type where typcategory='E' and typname = 'event_payment_type') then
    create type event_payment_type as enum (
      'upfront',
      'after_instance',
      'none'
    );
  end if;
end
$$;

alter table event drop column if exists registration_price;
alter table event add column if not exists payment_type event_payment_type not null default 'none';
alter table event add column if not exists use_default_price boolean not null default true;
alter table event add column if not exists member_price price null default null;
alter table event add column if not exists guest_price price null default null;

alter table tenant_trainer drop column if exists default_price;
alter table tenant_trainer add column if not exists member_price_45min price null default null;
alter table tenant_trainer add column if not exists guest_price_45min price null default null;
