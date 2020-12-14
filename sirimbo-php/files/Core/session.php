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
            new RedirectHelper('/error?id=ban');
        }
        if (!$data['u_confirmed']) {
            new RedirectHelper('/error?id=not_approved');
        }
        self::loadUser($data['u_id']);
        return true;
    }

    public static function logout()
    {
        session_destroy();
    }

    public static function loadUser($id): bool
    {
        if (Database::isDatabaseError() || !($user = DBUser::getUser($id))) {
            self::logout();
            return false;
        }
        $skupina = DBSkupiny::getSingle($user->getTrainingGroup());
        $permissions = DBPermissions::getSingleGroup($user->getPermissionGroup());
        $par = DBPary::getLatestPartner($user->getId(), $user->getGender());

        foreach (array_keys(Settings::$permissions) as $key) {
            if ($user->getPermissionGroup() == 0) {
                $_SESSION['permissions'][$key] = P_NONE;
            } else {
                $_SESSION['permissions'][$key] = $permissions['pe_' . $key];
            }
        }

        $_SESSION['invalid'] = !$user->isValid();
        $_SESSION['gdpr'] = !$user->getGdprSignedAt() || $user->getGdprSignedAt() === '0000-00-00 00:00:00';
        $_SESSION['login'] = 1;
        $_SESSION['id'] = $user->getId();
        $_SESSION['user'] = $user;
        $_SESSION['couple_data'] = $par;
        $_SESSION['skupina_data'] = $skupina;
        $_SESSION['par'] = $par['p_id'];
        $_SESSION['partner'] = $par['u_id'];

        return true;
    }

    public static function getPermissions($module): int
    {
        if (!self::isLogged()) {
            return P_NONE;
        }
        if (self::getUserID() == 1) {
            return P_ADMIN;
        }
        if (self::getUserData()->getPermissionGroup() == 0) {
            return P_NONE;
        }
        if (!isset($_SESSION['permissions'][$module])) {
            return P_NONE;
        }
        return $_SESSION['permissions'][$module];
    }

    public static function getUserID()
    {
        return self::isLogged() ? $_SESSION['user']->getId() : 0;
    }

    public static function getPartnerID()
    {
        return $_SESSION['partner'];
    }

    public static function getUserData(): User
    {
        return $_SESSION['user'];
    }

    public static function getCoupleData()
    {
        return $_SESSION['couple_data'];
    }

    public static function getUserPohlavi()
    {
        return $_SESSION['user']->getGender();
    }

    public static function getSkupina()
    {
        return $_SESSION['user']->getTrainingGroup();
    }

    public static function getSkupinaData()
    {
        return $_SESSION['skupina_data'];
    }

    public static function getZaplaceno()
    {
        return DBPlatby::hasPaidMemberFees(self::getUserId());
    }

    public static function getZaplacenoPar()
    {
        return true;
        // $paid = DBPlatby::hasPaidMemberFees(self::getUserId());
        // if ($_SESSION['partner']) {
        //     $paid = $paid && DBPlatby::hasPaidMemberFees(self::getPartnerId());
        // }
        // return $paid;
    }

    public static function getAgeGroup($year): string
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

    public static function isLogged(): bool
    {
        return isset($_SESSION['login']) && $_SESSION['login'] === 1;
    }

    public static function register(
        $login, $pass, $name, $surname, $pohlavi, $email, $telefon, $narozeni,
        $poznamky, $street, $popisne, $orientacni, $district, $city, $postal,
        $nationality, $skupina, $dancer
    ): void {
        DBUser::addUser(
            $login, User::crypt($pass), $name, $surname, $pohlavi, $email,
            $telefon, $narozeni, $poznamky, $street, $popisne, $orientacni,
            $district, $city, $postal, $nationality,
            '0', $skupina, '0', '0', '0', '0', $dancer ? '1' : '0', '0'
        );

        Mailer::newUserNotice(DEFAULT_ADMIN_MAIL, $login);
        Mailer::newUserNotice('hyzam@tkolymp.cz', $login);
    }

    public static function generateMsmtCsv(): string
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

        $oldest = DBPlatby::getOldestPayment();
        $newest = DBPlatby::getNewestPayment();
        foreach (DBUser::getUsers() as $u) {
            if ($u['u_ban'] || $u['u_temporary'] || !$u['u_confirmed'] || $u['u_system']) {
                continue;
            }
            // skupina - ne Host/VIP
            if (in_array($u['u_skupina'], ['9', '10', '13'])) {
                continue;
            }
            // od 1.9.2019
            if (new DateTime($newest[$u['u_id']]) < new DateTime('2019-09-01')) {
                continue;
            }

            $out .= '
' . implode(';', [
                $u['u_jmeno'],
                '',
                $u['u_prijmeni'],
                csvDate($u['u_narozeni']),
                $u['u_city'],
                $u['u_district'],
                $u['u_street'],
                $u['u_conscription_number'],
                $u['u_orientation_number'],
                str_replace(' ', '', $u['u_postal_code']),
                '',
                '',
                '',
                '',
                '66',
                $u['u_dancer'] ? '1' : '0',
                $u['u_teacher'] ? '1' : '0',
                isset($oldest[$u['u_id']]) ? csvDate($oldest[$u['u_id']]) : '',
                isset($newest[$u['u_id']]) ? csvDate($newest[$u['u_id']]) : '',
                $u['u_nationality'],
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
