--! Previous: sha1:f33659e8bb7235f6e98b4522c90cb15a94dcbf85
--! Hash: sha1:a606623ac50e1c7ea1d595dabe55c80735b56453

create or replace function current_user_id() returns text as $$
  SELECT current_setting('jwt.claims.user_id', true);
$$ language sql stable;

create or replace function get_current_user() returns users as $$
  SELECT * FROM users WHERE u_id = nullif(current_setting('jwt.claims.user_id', true), '')::integer;
$$ language sql stable;
