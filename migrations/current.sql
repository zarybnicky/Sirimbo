-- Write your migration here

create or replace function verify_function(f regproc, relid regclass DEFAULT 0) returns void as $$
declare
  error text[];
  count int;
begin
  select array_agg(plpgsql_check_function) into error
  from plpgsql_check_function(
    f,
    relid,
    performance_warnings => true,
    extra_warnings => true,
    security_warnings => true,
    compatibility_warnings => true
  );
  if array_length(error, 1) > 0 then
    raise exception using message = array_to_string(error, E'\n');
  end if;
end;
$$ language plpgsql volatile security definer;

create or replace function nabidka_my_lessons(n nabidka) returns int as $$
  select COALESCE(ni_pocet_hod, 0) from nabidka_item where n.n_id = ni_id_rodic
  and ni_partner in (select * from current_couple_ids());
$$ language sql stable;
grant execute on function nabidka_my_lessons to anonymous;

create or replace function nabidka_free_lessons(n nabidka) returns int as $$
  select n.n_pocet_hod - (select sum(ni_pocet_hod) from nabidka_item where ni_id_rodic = n.n_id);
$$ language sql stable;
grant execute on function nabidka_free_lessons to anonymous;

create or replace function users_full_name(u users) returns text as $$
  select trim(both from COALESCE(u.u_jmeno, '') || ' ' || COALESCE(u.u_prijmeni, ''));
$$ language sql stable;
grant execute on function users_full_name to anonymous;



ALTER TABLE ONLY public.nabidka_item
drop constraint if exists nabidka_item_unique_user_nabidka_key,
ADD CONSTRAINT nabidka_item_unique_user_nabidka_key UNIQUE (ni_partner, ni_id_rodic);


create or replace function reservation_set_desired_lessons(
  reservation_id bigint, lesson_count int,
  out reservation nabidka
) as $$
begin
  select * into reservation from nabidka where n_id = reservation_id;

  if lesson_count = 0 then
    delete from nabidka_item where ni_id_rodic = reservation_id and ni_partner in (select * from current_couple_ids());
    return;
  end if;

  if lesson_count > (nabidka_my_lessons(reservation) + nabidka_free_lessons(reservation)) then
    select (nabidka_my_lessons(reservation) + nabidka_free_lessons(reservation)) into lesson_count;
  end if;
  if reservation.n_max_pocet_hod > 0 and lesson_count > reservation.n_max_pocet_hod then
    select reservation.n_max_pocet_hod into lesson_count;
  end if;

  INSERT INTO nabidka_item
    (ni_id_rodic, ni_partner, ni_pocet_hod)
  values
    (reservation_id, (select current_couple_ids() limit 1), lesson_count)
  ON CONFLICT (ni_id_rodic, ni_partner)
  DO UPDATE SET ni_pocet_hod = lesson_count;

  select * into reservation from nabidka where n_id = reservation_id;
end;
$$ language plpgsql strict volatile security definer;
select verify_function('reservation_set_desired_lessons');
grant execute on function reservation_set_desired_lessons to member;


alter table skupiny alter column s_color_text set default '';

alter table skupiny drop column if exists internal_info cascade;
alter table skupiny add column internal_info jsonb not null default '[]'::jsonb;


create or replace function public.reservations_for_range(start_date date, end_date date) returns setof nabidka as $$
  select * from nabidka
  where n_visible=true
  and n_do >= start_date and n_od <= end_date
  order by n_od asc;
$$ language sql stable;
grant execute on function public.reservations_for_range TO member;
