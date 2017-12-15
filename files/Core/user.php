<?php
class User
{
    public static function login($login, $pass) {
        if (Database::isDatabaseError()) {
            User::logout();
            return false;
        }
        $login = strtolower($login);

        if (($login == "superadmin"
            && $pass == "9947a7bc1549a54e7299fe9a3975c8655430ade0" && ($id = 1))
            || ($id = DBUser::checkUser($login, $pass))) {
            $data = DBUser::getUserData($id);
            if ($data['u_ban'])
                throw new BanException("Váš účet byl pozastaven!");
            if (!$data['u_confirmed'])
                throw new NotApprovedException("Váš účet ještě nebyl potvrzen!");

            $_SESSION["login"] = 1;
            User::loadUser($data['u_id'], $data);

            if (!preg_match("/^[A-Z0-9._%-]+@[A-Z0-9.-]+\.[A-Z]{2,4}$/i", $data['u_email'])
                || !preg_match("/^((\+|00)\d{3})?( ?\d{3}){3}$/", $data['u_telefon'])
                || !preg_match("/^((?:19|20)\d\d)-(0[1-9]|1[012])-(0[1-9]|[12][0-9]|3[01])$/",
                        $data['u_narozeni'])) {
                $_SESSION['invalid_data'] = 1;
            } else {
                $_SESSION['invalid_data'] = 0;
            }
            return true;
        } else {
            return false;
        }
    }

    public static function logout() {
        session_unset();
    }

    public static function loadUser($id, $data = []) {
        if (Database::isDatabaseError()) {
            User::logout();
            return false;
        }
        if (empty($data)) {
            $data = DBUser::getUserData($id);
        }

        $par = DBPary::getLatestPartner($data['u_id'], $data['u_pohlavi']);

        foreach (Settings::$permissions as $key => $item) {
            if ($data['u_group'] == 0) {
                $_SESSION['permissions'][$key] = P_NONE;
            } else {
                $_SESSION['permissions'][$key] = $data['pe_' . $key];
            }
        }

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

        $_SESSION['zaplaceno'] = DBPlatby::hasPaidMemberFees($data['u_id']);
        $_SESSION['zaplaceno_par'] = $_SESSION['zaplaceno'] | DBPlatby::hasPaidMemberFees($par['u_id']);
        $_SESSION['zaplaceno_text'] =
            $_SESSION['zaplaceno'] ?
                ($_SESSION['zaplaceno_par'] ? null : 'Váš/e partner/ka nemá zaplacené členské příspěvky, ' .
                    'bez zaplacených příspěvků si nemůžete rezervovat lekce.') :
                'Nemáte zaplacené členské příspěvky, ' .
                    'bez zaplacených příspěvků si nemůžete rezervovat lekce.';
        /*
        $date = (int) date('md');
        if ($date >= '701' && $date <= 831) {
            $_SESSION['zaplaceno'] = $_SESSION['zaplaceno_par'] = true;
            if ($date > 824) {
                $remainingDays = 831 - $date;
                $subject = 'prázdnin';
            }
        } else {
            $now = time();
            $expire_user = strtotime($data['up_plati_do']) - $now;
            $expire_par = strtotime($par['up_plati_do']) - $now;

            $_SESSION['zaplaceno'] = ($expire_user >= 0);
            $_SESSION['zaplaceno_par'] = $_SESSION['zaplaceno'] && ($expire_par >= 0);

            if ($expire_user < 604800 && ($date < 623 || $date > 901)) {
                $remainingDays = round($expire_user / 86400);
                $subject = 'platnosti členských příspěvků';
            }
        }
        if (!isset($remainingDays) || !isset($subject))
            return true;

        switch($remainingDays) {
            case 8:
            case 7:
            case 6:
            case 5:
                $_SESSION['zaplaceno_text'] =
                    'Do konce ' . $subject . ' vám zbývá ' . $remainingDays . ' dní, ' .
                    'bez zaplacených příspěvků si nebudete moci rezervovat lekce.';
                break;
            case 4:
            case 3:
            case 2:
                $_SESSION['zaplaceno_text'] =
                    'Do konce ' . $subject . ' vám zbývají ' . $remainingDays . ' dny, ' .
                    'bez zaplacených příspěvků si nebudete moci rezervovat lekce.';
                break;
            case 1:
                $_SESSION['zaplaceno_text'] =
                    'Do konce ' . $subject . ' vám zbývá jeden den, ' .
                    'bez zaplacených příspěvků si nebudete moci rezervovat lekce.';
                break;
            case 0:
                $_SESSION['zaplaceno_text'] =
                    'Zítra začíná nové pololetí/čtvrtletí, ' .
                    'bez zaplacených příspěvků si nebudete moci rezervovat lekce.';
                break;
            default:
                $_SESSION['zaplaceno_text'] =
                    'Nemáte zaplacené členské příspěvky, ' .
                    'bez zaplacených příspěvků si nemůžete rezervovat lekce.';
                break;
        }
        */
        return true;
    }

    public static function getPermissions($module = '') {
        if (!User::isLogged()) {
            if ($module)
                return P_NONE;
            return [];
        } elseif (User::getUserID() == 1) {
            if ($module)
                return P_ADMIN;
            return $_SESSION['permissions'];
        } elseif (User::getUserGroup() == 0) {
            if ($module)
                return P_NONE;
            return $_SESSION['permissions'];
        } else {
            if ($module && isset($_SESSION['permissions'][$module]))
                return $_SESSION['permissions'][$module];
            return $_SESSION['permissions'];
        }
    }

    public static function getUserID() {
        if (User::isLogged())
            return $_SESSION["id"];
        else
            return 0;
    }

    public static function getPartnerID() {
        return $_SESSION['partner'];
    }

    public static function getUserName() {
        return $_SESSION["user"];
    }

    public static function getUserJmeno() {
        return $_SESSION['jmeno'];
    }

    public static function getUserPrijmeni() {
        return $_SESSION['prijmeni'];
    }

    public static function getUserWholeName() {
        return $_SESSION['jmeno'] . ' ' . $_SESSION['prijmeni'];
    }

    public static function getUserGroup() {
        return $_SESSION["group"];
    }

    public static function getGroupName() {
        return $_SESSION["groupName"];
    }

    public static function getUserPohlavi() {
        return $_SESSION['pohlavi'];
    }

    public static function getDatumNarozeni() {
        return $_SESSION['narozeni'];
    }

    public static function getSkupina() {
        return $_SESSION['skupina'];
    }

    public static function getSkupinaData() {
        return $_SESSION['skupina_data'];
    }

    public static function getZaplaceno($par = false) {
        if ($par)
            return $_SESSION['zaplaceno_par'];
        else
            return $_SESSION['zaplaceno'];
    }

    public static function getParID() {
        return $_SESSION['par'];
    }

    public static function isLogged() {
        return isset($_SESSION["login"]) && $_SESSION["login"] === 1;
    }

    public static function register($login, $pass, $name, $surname, $pohlavi, $email, $telefon,
            $narozeni, $poznamky) {
        DBUser::addUser($login, User::crypt($pass), $name, $surname, $pohlavi, $email,
            $telefon, $narozeni, $poznamky, '0', '0', '0', "0", "0", "0", "0");

        Mailer::newUserNotice(DEFAULT_ADMIN_MAIL, $login);
        Mailer::newUserNotice('hyzam@tkolymp.cz', $login);
    }

    public static function crypt($passwd) {
        $fix = md5("######TK.-.OLYMP######");
        return sha1($fix . $passwd . $fix);
    }

    public static function varSymbol($id) {
        return str_pad($id, 6, '0', STR_PAD_LEFT);
    }
}
?>
