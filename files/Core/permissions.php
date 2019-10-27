<?php
class Permissions
{
    protected static $request;

    public static function setRequest($request)
    {
        static::$request = $request;
    }

    public static function check($module, $level, $owner = null)
    {
        $l = Session::getPermissions($module);
        if ($l == P_OWNED && $level == P_OWNED && $owner != null) {
            return Session::getUserID() == $owner;
        }
        return $l >= $level;
    }

    public static function checkError($module, $level, $owner = null)
    {
        if (Permissions::check($module, $level, $owner)) {
            return true;
        }

        if (Session::isLogged()) {
            throw new AuthorizationException("Nemáte dostatečnou autorizaci pro tuto akci!");
        }
        (new RedirectHelper())->redirect(
            '/login?return=' . static::$request->server('REQUEST_URI'),
            'Nemáte dostatečná oprávnění k zobrazení požadovaného obsahu'
        );
    }
}
