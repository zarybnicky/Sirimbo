<?php
class DBUser extends Database
{
    public static function checkUser($login, $pass)
    {
        $res = self::query("SELECT * FROM users WHERE
            LOWER(u_login)='?' AND u_pass='?'", strtolower($login), $pass);
        if (!$res || !($data = self::getSingleRow($res))) {
            return false;
        }
        return $data["u_id"];
    }

    public static function getUserData($id): ?array
    {
        $res = self::query(
            "SELECT users.*, skupiny.*, permissions.* FROM users
                LEFT JOIN skupiny ON users.u_skupina=skupiny.s_id
                LEFT JOIN permissions ON u_group=pe_id
             WHERE u_id='?'",
            $id
        );
        if (!$res) {
            return null;
        }
        return self::getSingleRow($res);
    }

    public static function getUser(int $id): ?User
    {
        $res = self::query(
            "SELECT users.*, skupiny.*, permissions.* FROM users
                LEFT JOIN skupiny ON users.u_skupina=skupiny.s_id
                LEFT JOIN permissions ON u_group=pe_id
             WHERE u_id='?'",
            $id
        );
        if (!$res || !($row = self::getSingleRow($res))) {
            return null;
        }
        return User::fromArray($row);
    }

    public static function markGdprSigned($id)
    {
        self::query("UPDATE users SET u_gdpr_signed_at=NOW() WHERE u_id='?'", $id);
        return true;
    }

    public static function setPassword($id, $passwd)
    {
        self::query("UPDATE users SET u_pass='?' WHERE u_id='?'", $passwd, $id);
        return true;
    }

    public static function setUserData(
        $id, $jmeno, $prijmeni, $pohlavi, $email, $telefon, $narozeni, $rodnecislo, $poznamky,
        $street, $popisne, $orientacni, $district, $city, $postal, $nationality,
        $group, $skupina, $lock, $ban, $system, $dancer, $trener, $memberSince,
        $memberUntil, $gdprSignedAt
    ) {
        self::query(
            "UPDATE users SET u_jmeno='?',u_prijmeni='?',u_pohlavi='?',u_email='?'," .
            "u_telefon='?',u_narozeni='?',u_rodne_cislo='?', u_poznamky='?',u_street='?',u_conscription_number='?'," .
            "u_orientation_number='?',u_district='?',u_city='?',u_postal_code='?'," .
            "u_nationality='?',u_group='?',u_skupina='?',u_lock='?',u_ban='?',u_system='?',u_dancer='?'," .
            "u_teacher='?',u_member_since=" . ($memberSince ? "'$memberSince'" : 'NULL') .
            ",u_member_until=" . ($memberUntil ? "'$memberUntil'" : 'NULL') .
            ",u_gdpr_signed_at=" . ($gdprSignedAt ? "'$gdprSignedAt'" : 'NULL') .
            " WHERE u_id='?'",
            $jmeno, $prijmeni, $pohlavi, $email, $telefon, $narozeni, $rodnecislo, $poznamky,
            $street, $popisne, $orientacni, $district, $city, $postal, $nationality,
            $group, $skupina, $lock, $ban, $system, $dancer, $trener, $id
        );
        return true;
    }

    public static function getUsers($group = null)
    {
        $res = self::query(
            "SELECT users.*, skupiny.* FROM users" .
            " LEFT JOIN skupiny ON users.u_skupina=skupiny.s_id" .
            (($group == null) ? '' : " WHERE u_group='$group'") .
            " ORDER BY u_prijmeni"
        );
        return self::getArray($res);
    }

    public static function getUsersByPohlavi($pohlavi)
    {
        $res = self::query(
            "SELECT * FROM users
                LEFT JOIN skupiny ON users.u_skupina=skupiny.s_id
            WHERE u_pohlavi='?' ORDER BY u_prijmeni",
            $pohlavi
        );
        return self::getArray($res);
    }

    public static function getUsersWithSkupinaPlatby()
    {
        $res = self::query(
            "SELECT * FROM (
               SELECT DISTINCT ON (u_id) * FROM users
                 JOIN skupiny on s_id=u_skupina
                 LEFT JOIN platby_group_skupina ON pgs_id_skupina=s_id
                 LEFT JOIN (SELECT * FROM platby_item
                   JOIN platby_category ON pc_id=pi_id_category
                   WHERE CURRENT_DATE >= pc_valid_from AND CURRENT_DATE <= pc_valid_to
                 ) pi ON u_id=pi_id_user
                 LEFT JOIN platby_category_group ON pcg_id_category=pc_id
                 LEFT JOIN platby_group ON pg_id=pgs_id_group
               WHERE
                 u_confirmed='1' AND u_ban='0' AND u_system='0' AND pg_type<>'0'
               ORDER BY u_id
            ) tt ORDER BY tt.s_id, tt.u_prijmeni"
        );
        return self::getArray($res);
    }

    public static function getUsersByPermission($module, $permission)
    {
        $res = self::query(
            "SELECT users.*, skupiny.* FROM users
                LEFT JOIN permissions ON u_group=pe_id
                LEFT JOIN skupiny ON u_skupina=s_id
            WHERE pe_$module >= '?'
            ORDER BY u_prijmeni",
            $permission,
        );
        return self::getArray($res);
    }

    public static function getActiveUsers($group = null)
    {
        $res = self::query(
            "SELECT users.*,skupiny.* FROM users
                LEFT JOIN skupiny ON users.u_skupina=skupiny.s_id
            WHERE u_system='0' AND u_confirmed='1' AND u_ban='0' " .
            ($group !== null ? "AND u_group='$group' " : '') .
                "ORDER BY u_prijmeni "
        );
        return self::getArray($res);
    }
}
