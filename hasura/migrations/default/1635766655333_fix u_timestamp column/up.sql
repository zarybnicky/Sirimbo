
alter table "public"."users" alter column "u_timestamp" set default now();

alter table "public"."users" alter column "u_timestamp" set not null;
