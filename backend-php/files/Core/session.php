<?php
class Session
{
    private static $user;

    public static function login($login, $pass)
    {
        $data = \Database::querySingle("SELECT * FROM users WHERE LOWER(u_login)='?' AND u_pass='?'", strtolower($login), $pass);
        if (!$data || !$data["u_id"]) {
            return false;
        }
        if ($data['u_ban']) {
            \Redirect::to('/error?id=ban');
        }
        if (!$data['u_confirmed']) {
            \Redirect::to('/error?id=not_approved');
        }
        self::loadUser($data['u_id']);
        return true;
    }

    public static function loadUser($id): User
    {
        if (!$user = \DBUser::getUser($id)) {
            session_destroy();
            \Redirect::to('/');
        }

        $_SESSION['id'] = $user->getId();
        \Database::query('set session "jwt.claims.user_id" = \'?\'', $user->getId());
        \Database::query('set session "jwt.claims.tenant_id" = \'1\'');
        return self::$user = $user;
    }

    public static function getUser(): \User | null
    {
        if (!($_SESSION['id'] ?? null)) {
            return null;
        }
        return self::$user;
    }
}
