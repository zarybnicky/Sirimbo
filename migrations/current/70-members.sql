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
    AND u_ban='0'
    AND u_system='0'
  ORDER BY s_name, u_email;

GRANT ALL ON public.members TO member;

create or replace function my_lessons() returns setof rozpis_item as $$
  select rozpis_item.*
  from public.rozpis_item
  inner join public.rozpis on (rozpis.r_id = rozpis_item.ri_id_rodic)
  left join public.pary on (rozpis_item.ri_partner = pary.p_id)
  where (
        rozpis.r_trener = current_user_id()
     or pary.p_id_partner = current_user_id()
     or pary.p_id_partnerka = current_user_id()
  ) and rozpis.r_visible = true
--  and rozpis.r_datum > NOW()
  order by rozpis.r_datum, rozpis_item.ri_od
$$ language sql stable;
