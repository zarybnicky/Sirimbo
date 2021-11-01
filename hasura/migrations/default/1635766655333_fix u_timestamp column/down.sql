
alter table "public"."users" alter column "u_timestamp" drop not null;

ALTER TABLE "public"."users" ALTER COLUMN "u_timestamp" drop default;
