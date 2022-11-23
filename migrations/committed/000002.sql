--! Previous: sha1:c92280a1a093afdd296ca43acd91153fd819a8cb
--! Hash: sha1:608af8b9b6516ab91d6e0884a1ae9920d1e14df0

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
  ), oldest_payments as (
    SELECT DISTINCT ON (pi_id_user) platby_item.* AS pi_date
    FROM platby_item
    ORDER BY pi_id_user, pi_date ASC
  ), newest_payments as (
    SELECT DISTINCT ON (pi_id_user) platby_item.* AS pi_date
    FROM platby_item
    ORDER BY pi_id_user, pi_date DESC
  )
  SELECT users.*, skupiny.*,
    (SELECT EXISTS (SELECT pi_id FROM current_payments WHERE pi_id_user=u_id)) as payment_valid,
    (SELECT pi_date FROM oldest_payments where pi_id_user=u_id) as oldest_payment,
    (SELECT pi_date FROM newest_payments where pi_id_user=u_id) as newest_payment
  FROM users
  INNER JOIN skupiny ON s_id=u_skupina
  WHERE u_confirmed='1'
    AND u_system='0'
    AND u_ban='0'
  ORDER BY s_name, u_email;

GRANT ALL ON public.members TO member;
