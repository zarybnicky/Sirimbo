<?php
class DBUser extends Database
{
    public static function getUserData($id): ?array
    {
        return \Database::querySingle("SELECT users.*, skupiny.* FROM users LEFT JOIN skupiny ON users.u_skupina=skupiny.s_id WHERE u_id='?'", $id);
    }

    public static function getUsersWithSkupinaPlatby()
    {
        return \Database::queryArray(
            "SELECT * FROM (
               SELECT DISTINCT ON (u_id) * FROM users
                 JOIN skupiny on s_id=u_skupina
                 LEFT JOIN platby_group_skupina ON pgs_id_skupina=s_id
                 LEFT JOIN (SELECT * FROM platby_item
                   JOIN platby_category ON pc_id=pi_id_category
                   WHERE CURRENT_DATE >= pc_valid_from AND CURRENT_DATE <= pc_valid_to
                 ) pi ON u_id=pi_id_user
                 LEFT JOIN platby_category_group ON pcg_id_category=pc_id
                 LEFT JOIN platby_group ON pg_id=pgs_id_group AND ((pg_id=pcg_id_group AND pi_id IS NOT NULL) OR (pi_id IS NULL))
               WHERE
                 u_confirmed='1' AND u_ban='0' AND u_system='0' AND ((pg_type='1' AND pi_id IS NOT NULL) OR (pi_id IS NULL AND (pg_type='1' OR pg_type IS NULL)))
               ORDER BY u_id
            ) tt ORDER BY tt.s_id, tt.u_prijmeni"
        );
    }

    public static function getUsersByPermission($module, $permission)
    {
        return \Database::queryArray(
            "SELECT users.*, skupiny.* FROM users LEFT JOIN permissions ON u_group=pe_id LEFT JOIN skupiny ON u_skupina=s_id WHERE pe_$module >= '?' ORDER BY u_prijmeni",
            $permission,
        );
    }
}
