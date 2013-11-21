<?php
namespace TKOlomouc\Model;

use TKOlomouc\Model\Database\Adapter;
use TKOlomouc\Model\Database\Pagable;

class DBUser extends Adapter implements Pagable
{
    public static function getInstance()
    {
        return new self();
    }

    public static function getPage($offset, $count, $options = null)
    {
        if (!isset($options['filter'])) {
            $options['filter'] = 'all';
        }
        if (!isset($options['sort'])) {
            $options['sort'] = 'prijmeni';
        }
        $q = "SELECT users.*,skupiny.* FROM users
            LEFT JOIN skupiny ON users.u_skupina=skupiny.s_id";

        switch($options['filter']) {
            case 'unconfirmed':
                $q .= " AND u_confirmed='0' AND u_ban='0'";
                break;
            case 'ban':
                $q .= " AND u_ban='1'";
                break;
            case 'dancer':
                $q .= " AND u_confirmed='1' AND u_ban='0' AND u_system='0' AND u_dancer='1'";
                break;
            case 'system':
                $q .= " AND u_confirmed='1' AND u_ban='0' AND u_system='1'";
                break;
       	    case 'all':
            default:
                if (is_numeric($options['filter'])) {
                    $q .= " AND u_confirmed='1' AND u_ban='0' AND u_system='0' AND u_group='{$options['filter']}'";
                } else {
                    $q .= " AND u_confirmed='1' AND u_ban='0' AND u_system='0'";
                }
        }
        switch($options['sort']) {
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

    public static function getCount($options = null)
    {
        if (!isset($options['filter'])) {
            $options['filter'] = 'all';
        }
        $q = "SELECT COUNT(*) FROM users WHERE 1=1";
        switch($options['filter']) {
            case 'unconfirmed':
                $q .= " AND u_confirmed='0' AND u_ban='0'";
                break;
            case 'ban':
                $q .= " AND u_ban='1'";
                break;
            case 'dancer':
                $q .= " AND u_confirmed='1' AND u_ban='0' AND u_system='0' AND u_dancer='1'";
                break;
            case 'system':
                $q .= " AND u_confirmed='1' AND u_ban='0' AND u_system='1'";
                break;
            case 'all':
            default:
                if (is_numeric($options['filter'])) {
                    $q .= " AND u_confirmed='1' AND u_ban='0' AND u_system='0' AND u_group='{$options['filter']}'";
                } else {
                    $q .= " AND u_confirmed='1' AND u_ban='0' AND u_system='0'";
                }
        }
        $q .= ' ORDER BY u_prijmeni';

        $res = self::query($q);
        $res = self::getSingleRow($res);
        return $res['COUNT(*)'];
    }

    public static function checkUser($login, $pass)
    {
        list($login, $pass) = self::escape($login, $pass);

        $res = self::query(
            "SELECT * FROM users WHERE
            LOWER(u_login)=LOWER('$login') AND u_pass='$pass'"
        );
        if (!$res) {
            return false;
        } else {
            $row = self::getSingleRow($res);
            return $row["u_id"];
        }
    }

    public static function getUserGroup($id)
    {
        list($id) = self::escape($id);

        $res = self::query("SELECT u_group FROM users WHERE u_id='$id'");
        if (!$res) {
            return false;
        } else {
            $row = self::getSingleRow($res);
            return $row["u_group"];
        }
    }

    public static function getUserID($login)
    {
        list($login) = self::escape($login);

        $res = self::query("SELECT u_id FROM users WHERE u_login='$login'");
        if (!$res) {
            return false;
        } else {
            $row = self::getSingleRow($res);
            return $row["u_id"];
        }
    }

    public static function getUserDataByNameEmail($login, $email)
    {
        list($login, $email) = self::escape($login, $email);

        $res = self::query(
            "SELECT * FROM users WHERE LOWER(u_login)=LOWER('$login') AND u_email='$email'"
        );
        if (!$res) {
            return false;
        } else {
            return self::getSingleRow($res);
        }
    }

    public static function getUserData($id)
    {
        list($id) = self::escape($id);

        $res = self::query(
            "SELECT users.*,skupiny.*,permissions.* FROM users
                LEFT JOIN skupiny ON users.u_skupina=skupiny.s_id
                LEFT JOIN permissions ON u_group=pe_id
            WHERE u_id='$id'"
        );
        if (!$res) {
            return false;
        } else {
            return self::getSingleRow($res);
        }
    }

    public static function getUserByFullName($jmeno, $prijmeni)
    {
        list($jmeno, $prijmeni) = self::escape($jmeno, $prijmeni);

        $res = self::query(
        "SELECT * FROM users
        WHERE u_jmeno LIKE '$jmeno' AND u_prijmeni LIKE '$prijmeni'
        ORDER BY u_id");
        if (!$res) {
            return false;
        } else {
            return self::getSingleRow($res);
        }
    }

    public static function addTemporaryUser($login, $jmeno, $prijmeni, $narozeni)
    {
        list($login, $jmeno, $prijmeni, $narozeni) =
            self::escape($login, $jmeno, $prijmeni, $narozeni);

        self::query(
            "INSERT INTO users
                (u_login,u_pass,u_jmeno,u_prijmeni,u_narozeni,u_confirmed,u_temporary,u_system,u_group)
            VALUES ('$login','','$jmeno','$prijmeni','$narozeni','1','1','1','0')"
        );
        $user_id = mysql_insert_id();

        self::query("INSERT INTO pary (p_id_partner, p_archiv) VALUES ('" . $user_id . "','0')");
        $par_id = mysql_insert_id();

        return array($user_id, $par_id);
    }

    public static function isUserLocked($id)
    {
        list($id) = self::escape($id);

        $res = self::query("SELECT u_lock FROM users WHERE u_id='$id'");
        if (!$res) {
            return false;
        } else {
            $row = self::getSingleRow($res);
            return (bool) $row["u_lock"];
        }
    }

    public static function isUserConfirmed($id)
    {
        list($id) = self::escape($id);

        $res = self::query("SELECT u_confirmed FROM users WHERE u_id='$id'");
        if (!$res) {
            return false;
        } else {
            $row = self::getSingleRow($res);
            return (bool) $row["u_confirmed"];
        }
    }

    public static function confirmUser($id, $group, $skupina = '1', $dancer = 0)
    {
        list($id, $group, $skupina, $dancer) =
            self::escape($id, $group, $skupina, $dancer);

        self::query(
            "UPDATE users SET u_confirmed='1',u_group='$group',
            u_skupina='$skupina',u_dancer='$dancer',u_system='0' WHERE u_id='$id'"
        );
    }

    public static function setPassword($id, $passwd)
    {
        list($id, $passwd) = self::escape($id, $passwd);

        self::query("UPDATE users SET u_pass='$passwd' WHERE u_id='$id'");
        return true;
    }

    public static function setUserData(
        $id,
        $jmeno,
        $prijmeni,
        $pohlavi,
        $email,
        $telefon,
        $narozeni,
        $poznamky,
        $group,
        $skupina,
        $dancer,
        $lock,
        $ban,
        $system
    ) {
        list(
            $id, $jmeno, $prijmeni, $pohlavi, $email, $telefon, $narozeni,
            $poznamky, $group, $skupina, $dancer, $lock, $ban, $system
        ) = self::escape(
            $id, $jmeno, $prijmeni, $pohlavi, $email, $telefon, $narozeni,
            $poznamky, $group, $skupina, $dancer, $lock, $ban, $system
        );

        self::query(
            "UPDATE users SET
            u_jmeno='$jmeno',u_prijmeni='$prijmeni',u_pohlavi='$pohlavi',u_email='$email',
            u_telefon='$telefon',u_narozeni='$narozeni',u_poznamky='$poznamky',u_group='$group',
            u_skupina='$skupina',u_dancer='$dancer',u_lock='$lock',u_ban='$ban',u_system='$system'
            WHERE u_id='$id'"
        );
        return true;
    }

    public static function addUser(
        $login,
        $pass,
        $jmeno,
        $prijmeni,
        $pohlavi,
        $email,
        $telefon,
        $narozeni,
        $poznamky,
        $group,
        $skupina,
        $dancer,
        $lock,
        $ban,
        $confirmed,
        $system
    ) {

        list(
            $login, $pass, $jmeno, $prijmeni, $pohlavi, $email, $telefon, $narozeni,
            $poznamky, $group, $skupina, $dancer, $lock, $ban, $confirmed, $system
        ) = self::escape(
            $login, $pass, $jmeno, $prijmeni, $pohlavi, $email, $telefon, $narozeni,
            $poznamky, $group, $skupina, $dancer, $lock, $ban, $confirmed, $system
        );

        self::query(
            "INSERT INTO users
            (u_login,u_pass,u_jmeno,u_prijmeni,u_pohlavi,u_email,u_telefon,u_narozeni,
            u_poznamky,u_group,u_skupina,u_dancer,u_lock,u_ban,u_confirmed,u_system) VALUES
            ('$login','$pass','$jmeno','$prijmeni','$pohlavi','$email','$telefon','$narozeni',
            '$poznamky','$group','$skupina','$dancer','$lock','$ban','$confirmed','$system')"
        );
        self::query(
            "INSERT INTO pary (p_id_partner) VALUES
            ((SELECT u_id FROM users WHERE u_login='$login'))"
        );
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
            "SELECT users.*,skupiny.* FROM users
            LEFT JOIN skupiny ON users.u_skupina=skupiny.s_id"
                . (($group == null || $group == L_ALL) ? '' : " WHERE u_group='$group'")
                . " ORDER BY u_prijmeni"
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
            "SELECT * FROM users
                LEFT JOIN skupiny on s_id=u_skupina
                LEFT JOIN platby_group_skupina ON pgs_id_skupina=s_id
                LEFT JOIN platby_group ON pg_id=pgs_id_group
                LEFT JOIN platby_category_group ON pcg_id_group=pg_id
                LEFT JOIN platby_category ON pcg_id_category=pc_id
                LEFT OUTER JOIN platby_item ON u_id=pi_id_user
            WHERE
                u_confirmed='1' AND u_ban='0' AND u_system='0' AND
                (pg_type='1' OR pg_type IS NULL) AND (
                    (CURDATE() >= pc_valid_from OR CURDATE() <= pc_valid_to) OR
                    (pc_use_prefix='1' AND pi_prefix = YEAR(CURDATE()) AND
                        DATE_SUB(CURDATE(), INTERVAL (YEAR(CURDATE())) YEAR) <= pc_valid_from AND
                        DATE_SUB(CURDATE(), INTERVAL (YEAR(CURDATE())) YEAR) >= pc_valid_to) OR
                    pc_id IS NULL
                ) AND (pi_id_category=pc_id OR pi_id IS NULL)
            GROUP BY u_id
            ORDER BY s_id,u_prijmeni"
        );
        return self::getArray($res);
    }

    public static function getUsersByPermission($module, $permission)
    {
        list($module, $permission) = self::escape($module, $permission);
        $res = self::query(
            "SELECT users.*,skupiny.* FROM users
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

    public static function getActiveUsers($group = L_ALL)
    {
        $res = self::query(
            "SELECT users.*,skupiny.* FROM users
                LEFT JOIN skupiny ON users.u_skupina=skupiny.s_id
            WHERE u_system='0' AND u_confirmed='1' AND u_ban='0' " .
                ($group > L_ALL ? "AND u_group='$group' " : '') .
                "ORDER BY u_prijmeni"
        );
        return self::getArray($res);
    }

    public static function getActiveDancers($group = L_ALL)
    {
        $res = self::query(
            "SELECT users.*,skupiny.* FROM users
                LEFT JOIN skupiny ON users.u_skupina=skupiny.s_id
            WHERE u_system='0' AND u_dancer='1' AND u_confirmed='1' AND u_ban='0' " .
                ($group > -1 ? "AND u_group='$group' " : '') .
            "ORDER BY u_prijmeni"
        );
        return self::getArray($res);
    }

    public static function getGroupCounts()
    {
        $res = self::query(
            "SELECT u_group,count(*) as count,permissions.*
            FROM users LEFT JOIN permissions ON u_group=pe_id
            GROUP BY u_group"
        );
        return self::getArray($res);
    }
}
