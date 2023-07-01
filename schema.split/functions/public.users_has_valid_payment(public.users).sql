CREATE FUNCTION public.users_has_valid_payment(a public.users) RETURNS boolean
    LANGUAGE sql STABLE
    AS $$
  SELECT EXISTS (
    SELECT pi_id
    FROM platby_item
      INNER JOIN platby_category ON pi_id_category=pc_id
      INNER JOIN platby_category_group ON pcg_id_category=pc_id
      INNER JOIN platby_group ON pg_id=pcg_id_group
    WHERE pg_type='1'
      AND CURRENT_DATE >= pc_valid_from
      AND CURRENT_DATE <= pc_valid_to
      AND pi_id_user = a.u_id
  )
$$;

GRANT ALL ON FUNCTION public.users_has_valid_payment(a public.users) TO anonymous;


