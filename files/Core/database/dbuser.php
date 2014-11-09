<?php
class DBUser extends Database implements Pagable
{
    public static function getInstance() { return new self(); }

    public static function getPage($offset, $count, $options = null) {
        if (!isset($options['filter']))
            $options['filter'] = 'all';
        if (!isset($options['sort']))
            $options['sort'] = 'prijmeni';

        $q = "SELECT users.*,skupiny.* FROM users
            LEFT JOIN skupiny ON users.u_skupina=skupiny.s_id WHERE 1=1";
        switch($options['filter']) {
            case 'unconfirmed': $q .= " AND u_confirmed='0' AND u_ban='0'"; break;
            case 'ban': $q .= " AND u_ban='1'"; break;
            case 'dancer': $q .= " AND u_confirmed='1' AND u_ban='0' AND u_system='0' AND u_dancer='1'"; break;
            case 'system': $q .= " AND u_confirmed='1' AND u_ban='0' AND u_system='1'"; break;
       	    case 'all':
            default:
                if (is_numeric($options['filter']))
                    $q .= " AND u_confirmed='1' AND u_ban='0' AND u_system='0' AND u_group='{$options['filter']}'";
                else
                    $q .= " AND u_confirmed='1' AND u_ban='0' AND u_system='0'";
        }
        switch($options['sort']) {
            case 'var-symbol':    $q .= ' ORDER BY u_id'; break;
            case 'narozeni':    $q .= ' ORDER BY u_narozeni'; break;
            case 'prijmeni':
            default:            $q .= ' ORDER BY u_prijmeni'; break;
        }
        $q .= " LIMIT $offset,$count";
        $res = DBUser::query($q);
        return DBUser::getArray($res);
    }
    public static function getCount($options = null) {
        if (!isset($options['filter']))
            $options['filter'] = 'all';

        $q = "SELECT COUNT(*) FROM users WHERE 1=1";
        switch($options['filter']) {
            case 'unconfirmed': $q .= " AND u_confirmed='0' AND u_ban='0'"; break;
            case 'ban': $q .= " AND u_ban='1'"; break;
            case 'dancer': $q .= " AND u_confirmed='1' AND u_ban='0' AND u_system='0' AND u_dancer='1'"; break;
            case 'system': $q .= " AND u_confirmed='1' AND u_ban='0' AND u_system='1'"; break;
            case 'all':
            default:
                if (is_numeric($options['filter']))
                    $q .= " AND u_confirmed='1' AND u_ban='0' AND u_system='0' AND u_group='{$options['filter']}'";
                else
                    $q .= " AND u_confirmed='1' AND u_ban='0' AND u_system='0'";
        }
        $q .= ' ORDER BY u_prijmeni';

        $res = DBUser::query($q);
        $res = DBUser::getSingleRow($res);
        return $res['COUNT(*)'];
    }

    public static function checkUser($login, $pass) {
        list($login, $pass) = DBUser::escapeArray(array($login, $pass));

        $res = DBUser::query("SELECT * FROM users WHERE
            LOWER(u_login)=LOWER('$login') AND u_pass='$pass'");
        if (!$res) {
            return false;
        } else {
            $row = DBUser::getSingleRow($res);
            return $row["u_id"];
        }
    }

    public static function getUserGroup($id) {
        list($id) = DBUser::escapeArray(array($id));

        $res = DBUser::query("SELECT u_group FROM users WHERE " .
            "u_id='$id'");
        if (!$res) {
            return false;
        } else {
            $row = DBUser::getSingleRow($res);
            return $row["u_group"];
        }
    }

    public static function getUserID($login) {
        list($login) = DBUser::escapeArray(array($login));

        $res = DBUser::query("SELECT u_id FROM users WHERE " .
            "u_login='$login'");
        if (!$res) {
            return false;
        } else {
            $row = DBUser::getSingleRow($res);
            return $row["u_id"];
        }
    }

    public static function getUserDataByNameEmail($login, $email) {
        list($login, $email) = DBUser::escapeArray(array($login, $email));

        $res = DBUser::query("SELECT * FROM users WHERE LOWER(u_login)=LOWER('$login') AND u_email='$email'");
        if (!$res) {
            return false;
        } else {
            return DBUser::getSingleRow($res);
        }
    }

    public static function getUserData($id) {
        list($id) = DBUser::escape($id);

        $res = DBUser::query(
        "SELECT users.*,skupiny.*,permissions.* FROM users
            LEFT JOIN skupiny ON users.u_skupina=skupiny.s_id
            LEFT JOIN permissions ON u_group=pe_id
        WHERE u_id='$id'");
        if (!$res) {
            return false;
        } else {
            return DBUser::getSingleRow($res);
        }
    }

    public static function getUserByFullName($jmeno, $prijmeni) {
        list($jmeno, $prijmeni) = DBUser::escapeArray(array($jmeno, $prijmeni));

        $res = DBUser::query(
        "SELECT * FROM users
        WHERE u_jmeno LIKE '$jmeno' AND u_prijmeni LIKE '$prijmeni'
        ORDER BY u_id");
        if (!$res) {
            return false;
        } else {
            return DBUser::getSingleRow($res);
        }
    }
    public static function addTemporaryUser($login, $jmeno, $prijmeni, $narozeni) {
        list($login, $jmeno, $prijmeni, $narozeni) =
            DBUser::escapeArray(array($login, $jmeno, $prijmeni, $narozeni));

        DBUser::query(
        "INSERT INTO users
            (u_login,u_pass,u_jmeno,u_prijmeni,u_narozeni,u_confirmed,u_temporary,u_system,u_group)
        VALUES ('$login','','$jmeno','$prijmeni','$narozeni','1','1','1','0')");
        $user_id = mysql_insert_id();

        DBUser::query("INSERT INTO pary (p_id_partner, p_archiv) VALUES ('" . $user_id . "','0')");
        $par_id = mysql_insert_id();

        return array($user_id, $par_id);
    }

    public static function isUserLocked($id) {
        list($id) = DBUser::escapeArray(array($id));

        $res = DBUser::query("SELECT u_lock FROM users WHERE u_id='$id'");
        if (!$res) {
            return false;
        } else {
            $row = DBUser::getSingleRow($res);
            return (bool) $row["u_lock"];
        }
    }

    public static function isUserConfirmed($id) {
        list($id) = DBUser::escapeArray(array($id));

        $res = DBUser::query("SELECT u_confirmed FROM users WHERE u_id='$id'");
        if (!$res) {
            return false;
        } else {
            $row = DBUser::getSingleRow($res);
            return (bool) $row["u_confirmed"];
        }
    }

    public static function confirmUser($id, $group, $skupina = '1', $dancer = 0) {
        list($id, $group, $skupina, $dancer) =
            DBUser::escapeArray(array($id, $group, $skupina, $dancer));

        DBUser::query("UPDATE users SET u_confirmed='1',u_group='$group',
            u_skupina='$skupina',u_dancer='$dancer',u_system='0' WHERE u_id='$id'");
    }

    public static function setPassword($id, $passwd) {
        list($id, $passwd) = DBUser::escapeArray(array($id, $passwd));

        DBUser::query("UPDATE users SET u_pass='$passwd' WHERE u_id='$id'");
        return true;
    }

    public static function setUserData($id, $jmeno, $prijmeni, $pohlavi, $email, $telefon,
            $narozeni, $poznamky, $group, $skupina, $dancer, $lock, $ban, $system) {
        list($id, $jmeno, $prijmeni, $pohlavi, $email, $telefon, $narozeni, $poznamky, $group,
            $skupina, $dancer, $lock, $ban, $system) = DBUser::escapeArray(array($id, $jmeno,
            $prijmeni, $pohlavi, $email, $telefon, $narozeni, $poznamky, $group, $skupina,
            $dancer, $lock, $ban, $system));

        DBUser::query("UPDATE users SET " .
            "u_jmeno='$jmeno',u_prijmeni='$prijmeni',u_pohlavi='$pohlavi',u_email='$email'," .
            "u_telefon='$telefon',u_narozeni='$narozeni',u_poznamky='$poznamky',u_group='$group'," .
            "u_skupina='$skupina',u_dancer='$dancer',u_lock='$lock',u_ban='$ban',u_system='$system'" .
            " WHERE u_id='$id'");
        return true;
    }

    public static function addUser($login, $pass, $jmeno, $prijmeni, $pohlavi, $email, $telefon,
            $narozeni, $poznamky, $group, $skupina, $dancer, $lock, $ban, $confirmed, $system) {

        list($login, $pass, $jmeno, $prijmeni, $pohlavi, $email, $telefon, $narozeni, $poznamky,
            $group, $skupina, $dancer, $lock, $ban, $confirmed, $system) =
                DBUser::escapeArray(array($login, $pass, $jmeno, $prijmeni, $pohlavi, $email,
                $telefon, $narozeni, $poznamky, $group, $skupina, $dancer, $lock, $ban,
                $confirmed, $system));

        DBUser::query("INSERT INTO users " .
            "(u_login,u_pass,u_jmeno,u_prijmeni,u_pohlavi,u_email,u_telefon,u_narozeni," .
            "u_poznamky,u_group,u_skupina,u_dancer,u_lock,u_ban,u_confirmed,u_system) VALUES " .
            "('$login','$pass','$jmeno','$prijmeni','$pohlavi','$email','$telefon','$narozeni'," .
            "'$poznamky','$group','$skupina','$dancer','$lock','$ban','$confirmed','$system')");
        DBUser::query("INSERT INTO pary (p_id_partner) VALUES " .
            "((SELECT u_id FROM users WHERE u_login='$login'))");
        return true;
    }

    public static function removeUser($id) {
        list($id) = DBUser::escapeArray(array($id));

        DBUser::query("DELETE FROM users WHERE u_id='$id'");
        DBUser::query("DELETE FROM rozpis WHERE r_trener='$id'");
        DBUser::query("DELETE FROM rozpis_item WHERE ri_partner='$id'");
        DBUser::query("DELETE FROM nabidka WHERE n_trener='$id'");
        DBUser::query("DELETE FROM nabidka_item WHERE ni_partner='$id'");
        DBUser::query("DELETE FROM akce_item WHERE ai_user='$id'");

        DBPary::noPartner($id);
        DBUser::query("DELETE FROM pary WHERE p_id_partner='$id' AND p_archiv='0'");

        return true;
    }

    public static function getUsers($group = null) {
        $res = DBUser::query(
        "SELECT users.*,skupiny.* FROM users
            LEFT JOIN skupiny ON users.u_skupina=skupiny.s_id" .
        (($group == null) ? '' : " WHERE u_group='$group'") .
        " ORDER BY u_prijmeni");

        return DBUser::getArray($res);
    }

    public static function getUsersByPohlavi($pohlavi) {
        list($pohlavi) = DBUser::escapeArray(array($pohlavi));

        $res = DBUser::query(
        "SELECT * FROM users
            LEFT JOIN skupiny ON users.u_skupina=skupiny.s_id
        WHERE u_pohlavi='$pohlavi' ORDER BY u_prijmeni");
        return DBUser::getArray($res);
    }
            
    public static function getUsersWithSkupinaPlatby() {
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

    public static function getUsersByPermission($module, $permission) {
        list($module, $permission) = DBUser::escapeArray(array($module, $permission));
        $res = DBUser::query(
        "SELECT users.*,skupiny.* FROM users
            LEFT JOIN permissions ON u_group=pe_id
            LEFT JOIN skupiny ON u_skupina=s_id
        WHERE pe_$module >= '$permission'
        ORDER BY u_prijmeni");
        return DBUser::getArray($res);
    }

    public static function getNewUsers() {
        $res = DBUser::query(
        "SELECT * FROM users
            LEFT JOIN skupiny ON users.u_skupina=skupiny.s_id
        WHERE u_confirmed='0' ORDER BY u_prijmeni");
        return DBUser::getArray($res);
    }

    public static function getDuplicateUsers() {
        $res = DBUser::query(
        "SELECT u1.*,skupiny.* FROM users u1
            LEFT JOIN skupiny ON u1.u_skupina=skupiny.s_id
        WHERE EXISTS (SELECT * FROM users u2 WHERE
            ((u1.u_jmeno=u2.u_jmeno AND u1.u_prijmeni=u2.u_prijmeni) OR
            u1.u_email=u2.u_email OR u1.u_telefon=u2.u_telefon) AND u1.u_id!=u2.u_id)
        ORDER BY u_email, u_telefon, u_prijmeni");
        return DBUser::getArray($res);
    }

    public static function getBannedUsers() {
        $res = DBUser::query(
        "SELECT u1.*,skupiny.* FROM users u1
            LEFT JOIN skupiny ON u1.u_skupina=skupiny.s_id
        WHERE u_ban='1' ORDER BY u_prijmeni");
        return DBUser::getArray($res);
    }

    public static function getUnconfirmedUsers() {
        $res = DBUser::query(
        "SELECT u1.*,skupiny.* FROM users u1
            LEFT JOIN skupiny ON u1.u_skupina=skupiny.s_id
        WHERE u_banconfirmed='0' ORDER BY u_prijmeni");
        return DBUser::getArray($res);
    }

    public static function getActiveUsers($group = null) {
        $res = DBUser::query(
        "SELECT users.*,skupiny.* FROM users
            LEFT JOIN skupiny ON users.u_skupina=skupiny.s_id
        WHERE u_system='0' AND u_confirmed='1' AND u_ban='0' " .
        ($group !== null ? "AND u_group='$group' " : '') .
            "ORDER BY u_prijmeni ");
        return DBUser::getArray($res);
    }

    public static function getActiveDancers($group = null) {
        $res = DBUser::query(
        "SELECT users.*,skupiny.* FROM users
            LEFT JOIN skupiny ON users.u_skupina=skupiny.s_id
        WHERE u_system='0' AND u_dancer='1' AND u_confirmed='1' AND u_ban='0' " .
            ($group !== null ? "AND u_group='$group' " : '') .
        "ORDER BY u_prijmeni");
        return DBUser::getArray($res);
    }
    public static function getGroupCounts() {
        $res = DBUser::query(
        "SELECT u_group,count(*) as count,permissions.*
        FROM users LEFT JOIN permissions ON u_group=pe_id group by u_group");
        return DBUser::getArray($res);
    }
}
?>