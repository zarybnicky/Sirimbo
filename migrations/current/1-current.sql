alter table public.person
  add column if not exists instagram_username text,
  add column if not exists tiktok_username text,
  add column if not exists facebook_url text,
  add column if not exists website_url text;

--!include functions/create_person.sql
