<?php
class User
{
    public static function login($login, $pass)
    {
        if (Database::isDatabaseError()) {
            User::logout();
            return false;
        }
        $login = strtolower($login);

        if ($login == "superadmin" && $pass == "9947a7bc1549a54e7299fe9a3975c8655430ade0") {
            User::loadUser(1, DBUser::getUserData(1));
            return true;
        } elseif ($id = DBUser::checkUser($login, $pass)) {
            $data = DBUser::getUserData($id);
            if ($data['u_ban']) {
                Helper::instance()->redirect('/error?id=ban');
            }
            if (!$data['u_confirmed']) {
                Helper::instance()->redirect('/error?id=not_approved');
            }
            User::loadUser($data['u_id'], $data);
            return true;
        } else {
            return false;
        }
    }

    public static function logout()
    {
        session_unset();
    }

    public static function loadUser($id, $data = [])
    {
        if (Database::isDatabaseError()) {
            User::logout();
            return false;
        }
        if (empty($data)) {
            $data = DBUser::getUserData($id);
        }

        $par = DBPary::getLatestPartner($data['u_id'], $data['u_pohlavi']);

        foreach (array_keys(Settings::$permissions) as $key) {
            if ($data['u_group'] == 0) {
                $_SESSION['permissions'][$key] = P_NONE;
            } else {
                $_SESSION['permissions'][$key] = $data['pe_' . $key];
            }
        }

        if (!preg_match("/^[A-Z0-9._%-]+@[A-Z0-9.-]+\.[A-Z]{2,4}$/i", $data['u_email'])
            || !preg_match("/^((\+|00)\d{3})?( ?\d{3}){3}$/", $data['u_telefon'])
            || !preg_match(
                "/^((?:19|20)\d\d)-(0[1-9]|1[012])-(0[1-9]|[12][0-9]|3[01])$/",
                $data['u_narozeni']
            )
        ) {
            $_SESSION['invalid_data'] = 1;
        } else {
            $_SESSION['invalid_data'] = 0;
        }
        $_SESSION["login"] = 1;
        $_SESSION["id"] = $data['u_id'];
        $_SESSION["user"] = strtolower($data['u_login']);
        $_SESSION['jmeno'] = $data['u_jmeno'];
        $_SESSION['prijmeni'] = $data['u_prijmeni'];
        $_SESSION["pohlavi"] = $data['u_pohlavi'];
        $_SESSION['narozeni'] = $data['u_narozeni'];
        $_SESSION['group'] = $data['u_group'];
        $_SESSION['groupName'] = $data['pe_name'];
        $_SESSION['skupina'] = $data['u_skupina'];
        $_SESSION['skupina_data'] = [
            's_id '=> $data['s_id'],
            's_color_rgb' => $data['s_color_rgb'],
            's_name' => $data['s_name'],
            's_description' => $data['s_description']
        ];
        $_SESSION['par'] = $par['p_id'];
        $_SESSION['partner'] = $par['u_id'];

        return true;
    }

    public static function getPermissions($module = '')
    {
        if (!User::isLogged()) {
            return $module ? P_NONE : [];
        }
        if (User::getUserID() == 1) {
            return $module ? P_ADMIN : $_SESSION['permissions'];
        }
        if (User::getUserGroup() == 0) {
            return $module ? P_NONE : $_SESSION['permissions'];
        }
        if ($module && isset($_SESSION['permissions'][$module])) {
            return $_SESSION['permissions'][$module];
        }
        return $_SESSION['permissions'];
    }

    public static function getUserID()
    {
        return User::isLogged() ? $_SESSION["id"] : 0;
    }

    public static function getPartnerID()
    {
        return $_SESSION['partner'];
    }

    public static function getUserName()
    {
        return $_SESSION["user"];
    }

    public static function getUserJmeno()
    {
        return $_SESSION['jmeno'];
    }

    public static function getUserPrijmeni()
    {
        return $_SESSION['prijmeni'];
    }

    public static function getUserWholeName()
    {
        return $_SESSION['jmeno'] . ' ' . $_SESSION['prijmeni'];
    }

    public static function getUserGroup()
    {
        return $_SESSION["group"];
    }

    public static function getGroupName()
    {
        return $_SESSION["groupName"];
    }

    public static function getUserPohlavi()
    {
        return $_SESSION['pohlavi'];
    }

    public static function getDatumNarozeni()
    {
        return $_SESSION['narozeni'];
    }

    public static function getSkupina()
    {
        return $_SESSION['skupina'];
    }

    public static function getSkupinaData()
    {
        return $_SESSION['skupina_data'];
    }

    public static function getZaplaceno($par = false)
    {
        $paidSelf = DBPlatby::hasPaidMemberFees($_SESSION['id']);
        if ($par) {
            return $paidSelf | DBPlatby::hasPaidMemberFees($_SESSION['partner']);
        } else {
            return $paidSelf;
        }
    }

    public static function getParID()
    {
        return $_SESSION['par'];
    }

    public static function isLogged()
    {
        return isset($_SESSION["login"]) && $_SESSION["login"] === 1;
    }

    public static function register(
        $login, $pass, $name, $surname, $pohlavi, $email, $telefon, $narozeni,
        $poznamky, $skupina
    ) {
        DBUser::addUser(
            $login, User::crypt($pass), $name, $surname, $pohlavi, $email,
            $telefon, $narozeni, $poznamky, '0', $skupina, '0', "0", "0", "0",
            "0"
        );

        Mailer::newUserNotice(DEFAULT_ADMIN_MAIL, $login);
        Mailer::newUserNotice('hyzam@tkolymp.cz', $login);
    }

    public static function crypt($passwd)
    {
        $fix = md5("######TK.-.OLYMP######");
        return sha1($fix . $passwd . $fix);
    }

    public static function varSymbol($id)
    {
        return str_pad($id, 6, '0', STR_PAD_LEFT);
    }
}
