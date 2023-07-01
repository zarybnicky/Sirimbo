CREATE FUNCTION public.trainers() RETURNS SETOF public.users
    LANGUAGE sql STABLE
    AS $$
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
$$;

GRANT ALL ON FUNCTION public.trainers() TO member;


