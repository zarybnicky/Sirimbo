<?php
class Permissions
{
    protected static $request;

    public static function setRequest($request)
    {
        static::$request = $request;
    }

    public static function get($module)
    {
        return User::getPermissions($module);
    }

    public static function check($module, $level, $owner = null)
    {
        $l = User::getPermissions($module);
        if ($l == P_OWNED && $level == P_OWNED && $owner != null) {
            return User::getUserID() == $owner;
        }
        return $l >= $level;
    }

    public static function checkError($module, $level, $owner = null)
    {
        if (Permissions::check($module, $level, $owner)) {
            return true;
        }

        if (User::isLogged()) {
            throw new AuthorizationException("Nemáte dostatečnou autorizaci pro tuto akci!");
        }
        Helper::instance()->redirect(
            '/login?return=' . static::$request->server('REQUEST_URI'),
            'Nemáte dostatečná oprávnění k zobrazení požadovaného obsahu'
        );
    }
}
