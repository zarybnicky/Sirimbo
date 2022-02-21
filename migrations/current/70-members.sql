CREATE OR REPLACE VIEW public.members AS
  WITH current_payments as (
    SELECT * from platby_item
    INNER JOIN platby_category ON pi_id_category=pc_id
    INNER JOIN platby_category_group ON pcg_id_category=pc_id
    INNER JOIN platby_group ON pg_id=pcg_id_group
    WHERE pg_type='1'
    AND CURRENT_DATE >= pc_valid_from
    AND CURRENT_DATE <= pc_valid_to
  )
  SELECT u_email, s_name, (SELECT EXISTS (SELECT pi_id FROM current_payments WHERE pi_id_user=u_id)) as payment_valid
    FROM users INNER JOIN skupiny ON s_id=u_skupina
  WHERE u_confirmed='1'
    AND u_ban='0'
    AND u_system='0'
  ORDER BY s_name, u_email;
