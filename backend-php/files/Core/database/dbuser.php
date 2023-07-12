<?php
class DBUser extends Database
{
    public static function getUserData($id): ?array
    {
        return \Database::querySingle("SELECT users.*, skupiny.* FROM users LEFT JOIN skupiny ON users.u_skupina=skupiny.s_id WHERE u_id='?'", $id);
    }

    public static function addUser(
        $login, $pass, $jmeno, $prijmeni, $pohlavi, $email, $telefon, $narozeni, $rodnecislo, $poznamky,
        $street, $popisne, $orientacni, $district, $city, $postal, $nationality,
        $group, $skupina, $lock, $ban, $confirmed, $system, $dancer, $trener
    ) {
        \Database::query(
            "INSERT INTO users " .
            "(u_login,u_pass,u_jmeno,u_prijmeni,u_pohlavi,u_email,u_telefon,u_narozeni,u_rodne_cislo,u_poznamky," .
            "u_street,u_conscription_number,u_orientation_number,u_district,u_city,u_postal_code,u_nationality," .
            "u_group,u_skupina,u_lock,u_ban,u_confirmed,u_system,u_dancer,u_teacher,u_member_since,u_member_until," .
            "u_gdpr_signed_at) VALUES " .
            "('?','?','?','?','?','?','?','?','?','?','?','?','?','?','?','?','?','?','?','?','?','?','?','?','?',CURRENT_DATE,NULL,CURRENT_DATE)",
            $login, $pass, $jmeno, $prijmeni, $pohlavi, $email, $telefon, $narozeni, $rodnecislo, $poznamky,
            $street, $popisne, $orientacni, $district, $city, $postal, $nationality,
            $group, $skupina, $lock, $ban, $confirmed, $system, $dancer, $trener
        );
        \Database::query(
            "INSERT INTO pary (p_id_partner, p_id_partnerka) VALUES ('?', '0')",
            \Database::getInsertId(),
        );
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
