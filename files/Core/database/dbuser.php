<?php
class DBUser extends Database implements Pagable
{
    public function getPage($offset, $count, $options = null)
    {
        $q = "SELECT users.*, skupiny.* FROM users
            LEFT JOIN skupiny ON users.u_skupina=skupiny.s_id WHERE 1=1";
        switch ($options['status']) {
            case 'ban':
                $q .= " AND u_ban='1'";
                break;
            case 'system':
                $q .= " AND u_confirmed='1' AND u_ban='0' AND u_system='1'";
                break;
            case 'all':
            default:
                $q .= " AND u_confirmed='1' AND u_ban='0' AND u_system='0'";
        }

        if (is_numeric($options['group'])) {
            $q .= " AND u_group='{$options['group']}'";
        }
        if (is_numeric($options['skupina'])) {
            $q .= " AND u_skupina='{$options['skupina']}'";
        }

        switch ($options['sort']) {
            case 'var-symbol':
                $q .= ' ORDER BY u_id';
                break;
            case 'narozeni':
                $q .= ' ORDER BY u_narozeni';
                break;
            case 'prijmeni':
            default:
                $q .= ' ORDER BY u_prijmeni';
                break;
        }
        $q .= " LIMIT $offset,$count";
        $res = self::query($q);
        return self::getArray($res);
    }

    public function getCount($options = null)
    {
        if (!isset($options['status'])) {
            $options['status'] = 'all';
        }

        $q = "SELECT COUNT(*) FROM users WHERE 1=1";

        if (is_numeric($options['group'])) {
            $q .= " AND u_group='{$options['group']}'";
        }
        if (is_numeric($options['skupina'])) {
            $q .= " AND u_skupina='{$options['skupina']}'";
        }

        switch ($options['status']) {
            case 'unconfirmed':
                $q .= " AND u_confirmed='0' AND u_ban='0'";
                break;
            case 'ban':
                $q .= " AND u_ban='1'";
                break;
            case 'system':
                $q .= " AND u_confirmed='1' AND u_ban='0' AND u_system='1'";
                break;
            case 'all':
            default:
                $q .= " AND u_confirmed='1' AND u_ban='0' AND u_system='0'";
                break;
        }

        $res = self::query($q);
        $res = self::getSingleRow($res);
        return $res['COUNT(*)'];
    }

    public static function checkUser($login, $pass)
    {
        $res = self::query("SELECT * FROM users WHERE
            LOWER(u_login)='?' AND u_pass='?'", strtolower($login), $pass);
        if (!$res) {
            return false;
        }
        return self::getSingleRow($res)["u_id"];
    }

    public static function getUserID($login)
    {
        $res = self::query("SELECT u_id FROM users WHERE u_login='?'", $login);
        if (!$res) {
            return false;
        }
        $row = self::getSingleRow($res);
        return $row["u_id"];
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

    public static function getUserByNameEmail($login, $email): ?User
    {
        $res = self::query(
            "SELECT * FROM users WHERE LOWER(u_login)='?' AND LOWER(u_email)='?'",
            strtolower($login),
            strtolower($email)
        );
        if (!$res) {
            return null;
        }
        return User::fromArray(self::getSingleRow($res));
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
        if (!$res) {
            return null;
        }
        return User::fromArray(self::getSingleRow($res));
    }

    public static function confirmUser($id, $group, $skupina = '1')
    {
        self::query(
            "UPDATE users SET u_confirmed='1',u_group='?',u_skupina='?',u_system='0' WHERE u_id='?'",
            $group,
            $skupina,
            $id
        );
    }

    public static function setPassword($id, $passwd)
    {
        self::query("UPDATE users SET u_pass='?' WHERE u_id='?'", $passwd, $id);
        return true;
    }

    public static function setUserData(
        $id, $jmeno, $prijmeni, $pohlavi, $email, $telefon, $narozeni, $poznamky,
        $street, $popisne, $orientacni, $district, $city, $postal, $nationality,
        $group, $skupina, $lock, $ban, $system, $dancer, $trener, $memberSince,
        $memberUntil, $gdprSignedAt
    ) {
        self::query(
            "UPDATE users SET u_jmeno='?',u_prijmeni='?',u_pohlavi='?',u_email='?'," .
            "u_telefon='?',u_narozeni='?',u_poznamky='?',u_street='?',u_conscription_number='?'," .
            "u_orientation_number='?',u_district='?',u_city='?',u_postal_code='?'," .
            "u_nationality='?',u_group='?',u_skupina='?',u_lock='?',u_ban='?',u_system='?',u_dancer='?'," .
            "u_teacher='?',u_member_since='?',u_member_until='?',u_gdpr_signed_at='?' WHERE u_id='?'",
            $jmeno, $prijmeni, $pohlavi, $email, $telefon, $narozeni, $poznamky,
            $street, $popisne, $orientacni, $district, $city, $postal, $nationality,
            $group, $skupina, $lock, $ban, $system, $dancer, $trener, $memberSince,
            $memberUntil, $gdprSignedAt, $id
        );
        return true;
    }

    public static function addUser(
        $login, $pass, $jmeno, $prijmeni, $pohlavi, $email, $telefon, $narozeni, $poznamky,
        $street, $popisne, $orientacni, $district, $city, $postal, $nationality,
        $group, $skupina, $lock, $ban, $confirmed, $system, $dancer, $trener
    ) {
        self::query(
            "INSERT INTO users " .
            "(u_login,u_pass,u_jmeno,u_prijmeni,u_pohlavi,u_email,u_telefon,u_narozeni,u_poznamky," .
            "u_street,u_conscription_number,u_orientation_number,u_district,u_city,u_postal_code,u_nationality," .
            "u_group,u_skupina,u_lock,u_ban,u_confirmed,u_system,u_dancer,u_teacher,u_member_since,u_member_until," .
            "u_gdpr_signed_at) VALUES " .
            "('?','?','?','?','?','?','?','?','?','?','?','?','?','?','?','?','?','?','?','?','?','?','?','?',YEAR(CURDATE()),NULL,CURDATE())",
            $login, $pass, $jmeno, $prijmeni, $pohlavi, $email, $telefon, $narozeni, $poznamky,
            $street, $popisne, $orientacni, $district, $city, $postal, $nationality,
            $group, $skupina, $lock, $ban, $confirmed, $system, $dancer, $trener
        );
        $uid = self::getInsertId();
        self::query("INSERT INTO pary (p_id_partner, p_id_partnerka) VALUES ('$uid', '0')");
        return true;
    }

    public static function removeUser($id)
    {
        list($id) = self::escape($id);

        self::query("DELETE FROM users WHERE u_id='$id'");
        self::query("DELETE FROM rozpis WHERE r_trener='$id'");
        self::query("DELETE FROM rozpis_item WHERE ri_partner='$id'");
        self::query("DELETE FROM nabidka WHERE n_trener='$id'");
        self::query("DELETE FROM nabidka_item WHERE ni_partner='$id'");
        self::query("DELETE FROM akce_item WHERE ai_user='$id'");

        DBPary::noPartner($id);
        self::query("DELETE FROM pary WHERE p_id_partner='$id' AND p_archiv='0'");

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
        list($pohlavi) = self::escape($pohlavi);

        $res = self::query(
            "SELECT * FROM users
                LEFT JOIN skupiny ON users.u_skupina=skupiny.s_id
            WHERE u_pohlavi='$pohlavi' ORDER BY u_prijmeni"
        );
        return self::getArray($res);
    }

    public static function getUsersWithSkupinaPlatby()
    {
        $res = self::query(
            "SELECT *
            FROM users
                LEFT JOIN (
                    SELECT * FROM platby_item
                        JOIN platby_category ON pc_id=pi_id_category
                    WHERE
                        (pc_use_prefix='0' AND
                         CURDATE() >= pc_valid_from AND
                         CURDATE() <= pc_valid_to)
                        OR
                        (pc_use_prefix='1' AND
                         DATE_SUB(CURDATE(), INTERVAL (YEAR(CURDATE())) YEAR) <= pc_valid_from AND
                         DATE_SUB(CURDATE(), INTERVAL (YEAR(CURDATE())) YEAR) >= pc_valid_to)
                ) pi ON u_id=pi_id_user
                JOIN skupiny on s_id=u_skupina
                LEFT JOIN platby_group_skupina ON pgs_id_skupina=s_id
                LEFT JOIN platby_category_group ON pcg_id_category=pc_id
                LEFT JOIN platby_group ON pg_id=pgs_id_group AND ((pg_id=pcg_id_group AND pi_id IS NOT NULL) OR (pi_id IS NULL))
            WHERE
                u_confirmed='1' AND u_ban='0' AND u_system='0' AND ((pg_type='1' AND pi_id IS NOT NULL) OR (pi_id IS NULL AND (pg_type='1' OR pg_type IS NULL)))
            GROUP BY u_id
            ORDER BY s_id,u_prijmeni"
        );
        return self::getArray($res);
    }

    public static function getUsersByPermission($module, $permission)
    {
        list($module, $permission) = self::escape($module, $permission);
        $res = self::query(
            "SELECT users.*, skupiny.* FROM users
                LEFT JOIN permissions ON u_group=pe_id
                LEFT JOIN skupiny ON u_skupina=s_id
            WHERE pe_$module >= '$permission'
            ORDER BY u_prijmeni"
        );
        return self::getArray($res);
    }

    public static function getNewUsers()
    {
        $res = self::query(
            "SELECT * FROM users
                LEFT JOIN skupiny ON users.u_skupina=skupiny.s_id
            WHERE u_confirmed='0' ORDER BY u_prijmeni"
        );
        return self::getArray($res);
    }

    public static function getDuplicateUsers()
    {
        $res = self::query(
            "SELECT u1.*,skupiny.* FROM users u1
                LEFT JOIN skupiny ON u1.u_skupina=skupiny.s_id
            WHERE EXISTS (SELECT * FROM users u2 WHERE
                ((u1.u_jmeno=u2.u_jmeno AND u1.u_prijmeni=u2.u_prijmeni) OR
                u1.u_email=u2.u_email OR u1.u_telefon=u2.u_telefon) AND u1.u_id!=u2.u_id)
            ORDER BY u_email, u_telefon, u_prijmeni"
        );
        return self::getArray($res);
    }

    public static function getBannedUsers()
    {
        $res = self::query(
            "SELECT u1.*,skupiny.* FROM users u1
                LEFT JOIN skupiny ON u1.u_skupina=skupiny.s_id
            WHERE u_ban='1' ORDER BY u_prijmeni"
        );
        return self::getArray($res);
    }

    public static function getUnconfirmedUsers()
    {
        $res = self::query(
            "SELECT u1.*,skupiny.* FROM users u1
                LEFT JOIN skupiny ON u1.u_skupina=skupiny.s_id
            WHERE u_banconfirmed='0' ORDER BY u_prijmeni"
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
    public static function getGroupCounts()
    {
        $res = self::query(
            "SELECT u_group,count(*) as count,permissions.*
            FROM users LEFT JOIN permissions ON u_group=pe_id group by u_group"
        );
        return self::getArray($res);
    }
}
