SELECT DISTINCT ON (u_id) u_email FROM platby_item
INNER JOIN platby_category ON pc_id=pi_id_category
INNER JOIN platby_category_group ON pcg_id_category=pc_id
INNER JOIN platby_group ON pg_id=pcg_id_group
INNER JOIN platby_group_skupina ON pgs_id_group=pg_id
INNER JOIN users ON pi_id_user=u_id AND u_skupina=s_id
WHERE pg_type='1'
AND CURRENT_DATE >= pc_valid_from
AND CURRENT_DATE <= pc_valid_to
AND u_confirmed='1'
AND u_ban='0'
AND u_system='0';
