drop view if exists public.members cascade;
create or replace view public.members as
  WITH current_payments as (
    SELECT * from platby_item
    INNER JOIN platby_category ON pi_id_category=pc_id
    INNER JOIN platby_category_group ON pcg_id_category=pc_id
    INNER JOIN platby_group ON pg_id=pcg_id_group
    WHERE pg_type='1'
    AND CURRENT_DATE >= pc_valid_from
    AND CURRENT_DATE <= pc_valid_to
  )
  SELECT users.*, skupiny.*, (SELECT EXISTS (SELECT pi_id FROM current_payments WHERE pi_id_user=u_id)) as payment_valid
    FROM users INNER JOIN skupiny ON s_id=u_skupina
  WHERE u_confirmed='1'
    AND u_system='0'
    AND u_ban='0'
  ORDER BY s_name, u_email;

GRANT ALL ON public.members TO member;

create or replace function public.trainers() returns setof users as $$
  SELECT users.*
    FROM users INNER JOIN permissions on u_group=pe_id
  WHERE u_confirmed='1'
    AND u_system='0'
    AND u_ban='0'
    AND CASE
      WHEN (select pe_rozpis > 8 from current_permissions()) THEN pe_rozpis >= 8
      ELSE u_id = current_user_id()
    END
  ORDER BY u_prijmeni, u_jmeno;
$$ language sql stable;
GRANT EXECUTE ON function public.trainers TO member;

create or replace function my_lessons(start_date date, end_date date) returns setof rozpis_item as $$
  select rozpis_item.*
  from public.rozpis_item
  inner join public.rozpis on (rozpis.r_id = rozpis_item.ri_id_rodic)
  left join public.pary on (rozpis_item.ri_partner = pary.p_id)
  where (
        rozpis.r_trener = current_user_id()
     or pary.p_id_partner = current_user_id()
     or pary.p_id_partnerka = current_user_id()
  ) and rozpis.r_visible = true and r_datum >= start_date and r_datum <= end_date
  order by rozpis.r_datum, rozpis_item.ri_od
$$ language sql stable;
grant execute on function public.my_lessons TO member;

create or replace function public.schedules_for_range(start_date date, end_date date) returns setof rozpis as $$
  select * from rozpis
  where r_visible=true
  and r_datum >= start_date and r_datum <= end_date
  order by r_datum asc;
$$ language sql stable;
grant execute on function public.schedules_for_range TO member;

create or replace function public.reservations_for_range(start_date date, end_date date) returns setof nabidka as $$
  select * from nabidka
  where n_visible=true
  and n_od <= start_date and n_do >= end_date
  order by n_od asc;
$$ language sql stable;
grant execute on function public.reservations_for_range TO member;

create or replace function public.active_couples() returns setof pary as $$
  select p.*
  from pary as p
      left join users as m on p.p_id_partner=m.u_id
      left join users as f on p.p_id_partnerka=f.u_id
  where p.p_archiv = false
      and p.p_id_partner is not null and p.p_id_partner <> 0
      and p.p_id_partnerka is not null and p.p_id_partnerka <> 0
      and m.u_id is not null and f.u_id is not null
  order by m.u_prijmeni asc
$$ language sql stable;
grant execute on function public.active_couples TO member;


create or replace function public.create_couple(
  man bigint, woman bigint
) returns setof pary as $$
declare
  couple_man pary;
  couple_woman pary;
begin
  select * into couple_man from pary
  where p_archiv=false and p_id_partner=man;

  select * into couple_woman from pary
  where p_archiv=false and (p_id_partnerka=woman or (p_id_partnerka is null and p_id_partner=woman));

  if couple_man.p_id_partnerka = woman then
     return query select * from couple_man;
  end if;

  if couple_man.p_id_partnerka is not null and couple_man.p_id_partnerka<>0 then
    insert into pary (p_id_partner, p_id_partnerka) VALUES (couple_man.p_id_partnerka, 0);
  end if;
  update pary set p_archiv=true where p_id = couple_man.p_id;

  if couple_woman.p_id_partnerka is not null and couple_woman.p_id_partnerka<>0 then
    insert into pary (p_id_partner, p_id_partnerka) VALUES (couple_woman.p_id_partner, 0);
  end if;
  update pary set p_archiv=true where p_id = couple_woman.p_id;

  return query insert into pary (p_id_partner, p_id_partnerka) VALUES (man, woman) returning *;
end;
$$ language plpgsql strict volatile security definer;
select plpgsql_check_function('public.create_couple');
grant execute on function public.create_couple to administrator;

create or replace function public.fix_unpaired_couples() returns setof pary as $$
  insert into pary (p_id_partner, p_id_partnerka)
  select u_id, 0 from users
  where u_id not in (
    select u_id from users
    left join pary on p_id_partnerka=u_id or p_id_partner=u_id
    where p_archiv=false
  ) returning *;
$$ language sql strict volatile security definer;
grant execute on function public.fix_unpaired_couples to administrator;
