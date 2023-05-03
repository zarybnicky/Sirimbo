--! Previous: sha1:11ee8ffb56ed076b16d1d8a2064758fedaa9cf3c
--! Hash: sha1:85b3d021e236aa96ea55fd5da915dfaaf3b01051

-- Write your migration here

select app_private.drop_policies('public.users');
alter table users enable row level security;
create policy admin_all on users to administrator using (true) with check (true);
create policy all_view on users for select using (true);
create policy manage_own on users for all
  using (u_id = current_user_id()) with check (u_id = current_user_id());
create policy register_anonymous on users for insert with check (u_confirmed=false);

grant all on users to member;
grant insert (u_id, u_login, u_pass, u_jmeno, u_prijmeni, u_pohlavi, u_email,
u_telefon, u_narozeni, u_rodne_cislo, u_poznamky, u_dancer, u_street,
u_conscription_number, u_orientation_number, u_district, u_city,
u_postal_code, u_nationality) on users to anonymous;
grant select on users to anonymous;

comment on view public.akce_item is E'@foreignKey (ai_id_rodic) references akce (a_id)
@foreignKey (ai_user) references users (u_id)';

grant all on akce_item to anonymous;
grant all on akce to anonymous;
