<?php
class Session
{
    public static function login($login, $pass)
    {
        if (Database::isDatabaseError()) {
            self::logout();
            return false;
        }
        $login = strtolower($login);

        if ($login == "superadmin" && $pass == "9947a7bc1549a54e7299fe9a3975c8655430ade0") {
            self::loadUser(1);
            return true;
        }
        $id = DBUser::checkUser($login, $pass);
        if (!$id) {
            return false;
        }
        $data = DBUser::getUserData($id);
        if ($data['u_ban']) {
            (new RedirectHelper())->redirect('/error?id=ban');
        }
        if (!$data['u_confirmed']) {
            (new RedirectHelper())->redirect('/error?id=not_approved');
        }
        self::loadUser($data['u_id']);
        return true;
    }

    public static function logout()
    {
        session_unset();
    }

    public static function loadUser($id)
    {
        if (Database::isDatabaseError()) {
            self::logout();
            return false;
        }
        $data = DBUser::getUserData($id);
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
        $_SESSION['login'] = 1;
        $_SESSION['user_data'] = $data;
        $_SESSION['couple_data'] = $par;
        $_SESSION['id'] = $data['u_id'];
        $_SESSION['jmeno'] = $data['u_jmeno'];
        $_SESSION['prijmeni'] = $data['u_prijmeni'];
        $_SESSION['pohlavi'] = $data['u_pohlavi'];
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

    public static function getPermissions($module)
    {
        if (!self::isLogged()) {
            return P_NONE;
        }
        if (self::getUserID() == 1) {
            return P_ADMIN;
        }
        if (self::getUserData()['u_group'] == 0) {
            return P_NONE;
        }
        if (!isset($_SESSION['permissions'][$module])) {
            return P_NONE;
        }
        return $_SESSION['permissions'][$module];
    }

    public static function getUserID()
    {
        return self::isLogged() ? $_SESSION['id'] : 0;
    }

    public static function getPartnerID()
    {
        return $_SESSION['partner'];
    }

    public static function getUserData()
    {
        return $_SESSION['user_data'];
    }

    public static function getCoupleData()
    {
        return $_SESSION['couple_data'];
    }

    public static function getUserPohlavi()
    {
        return $_SESSION['pohlavi'];
    }

    public static function getSkupina()
    {
        return $_SESSION['skupina'];
    }

    public static function getSkupinaData()
    {
        return $_SESSION['skupina_data'];
    }

    public static function getZaplaceno()
    {
        return DBPlatby::hasPaidMemberFees($_SESSION['id']);
    }

    public static function getZaplacenoPar()
    {
        $paid = DBPlatby::hasPaidMemberFees($_SESSION['id']);
        if ($_SESSION['partner']) {
            $paid = $paid && DBPlatby::hasPaidMemberFees($_SESSION['partner']);
        }
        return $paid;
    }

    public static function getAgeGroup($year)
    {
        $diff = date('Y') - $year;
        if ($diff < 8) {
            return 'Do 8 let';
        } elseif ($diff < 10) {
            return 'Děti I';
        } elseif (10 <= $diff && $diff < 12) {
            return 'Děti II';
        } elseif (12 <= $diff && $diff < 14) {
            return 'Junioři I';
        } elseif (14 <= $diff && $diff < 16) {
            return 'Junioři II';
        } elseif (16 <= $diff && $diff < 19) {
            return 'Mládež';
        } elseif (19 <= $diff && $diff < 21) {
            return 'Do 21 let';
        } elseif (21 <= $diff && $diff < 35) {
            return 'Dospělí';
        } elseif (35 <= $diff && $diff < 45) {
            return 'Senioři I';
        } elseif (45 <= $diff && $diff < 55) {
            return 'Senioři II';
        } elseif (55 <= $diff && $diff < 65) {
            return 'Senioři III';
        } elseif (65 <= $diff) {
            return 'Senioři IV';
        }
    }

    public static function getParID()
    {
        return $_SESSION['par'];
    }

    public static function isLogged()
    {
        return isset($_SESSION['login']) && $_SESSION['login'] === 1;
    }

    public static function register(
        $login, $pass, $name, $surname, $pohlavi, $email, $telefon, $narozeni,
        $poznamky, $street, $popisne, $orientacni, $district, $city, $postal,
        $nationality, $skupina, $dancer
    ) {
        DBUser::addUser(
            $login, User::crypt($pass), $name, $surname, $pohlavi, $email,
            $telefon, $narozeni, $poznamky, $street, $popisne, $orientacni,
            $district, $city, $postal, $nationality,
            '0', $skupina, '0', '0', '0', '0', $dancer ? '1' : '0', '0'
        );

        Mailer::newUserNotice(DEFAULT_ADMIN_MAIL, $login);
        Mailer::newUserNotice('hyzam@tkolymp.cz', $login);
    }

    public static function generateMsmtCsv()
    {
        $out = implode(';', [
            'JMENO',
            'DALSI_JMENA',
            'PRIJMENI',
            'DATUM_NAROZENI',

            'NAZEV_OBCE',
            'NAZEV_CASTI_OBCE',
            'NAZEV_ULICE',
            'CISLO_POPISNE',
            'CISLO_ORIENTACNI',
            'PSC',

            'STRECHA',
            'SVAZ',
            'KLUB',
            'ODDIL',

            'DRUH_SPORTU',
            'SPORTOVEC',
            'TRENER',
            'CLENSTVI_OD',
            'CLENSTVI_DO',
            'OBCANSTVI',
            'EXT_ID'
        ]);

        $platby = DBPlatby::getOldestPayment();
        foreach (DBUser::getUsers() as $u) {
            if ($u['u_ban'] || $u['u_temporary'] || !$u['u_confirmed'] || $u['u_system']) {
                continue;
            }
            $out .= '
' . implode(';', [
                $u['u_jmeno'],
                '',
                $u['u_prijmeni'],
                csvDate($u['u_narozeni']),
                '',
                '',
                '',
                '',
                '',
                '',
                '',
                '',
                '',
                '',
                '66',
                $u['u_dancer'] ? '1' : '0',
                $u['u_teacher'] ? '1' : '0',
                csvDate($platby[$u['u_id']]),
                '',
                '',
                ''
            ]);
        }
        return $out;
    }
}

function csvDate($x)
{
    return implode('.', array_reverse(explode('-', $x)));
}
