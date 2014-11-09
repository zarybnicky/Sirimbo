<?php
class Permissions
{
    public static function get($module)
    {
        return User::getPermissions($module);
    }
    public static function check($module, $level, $owner = null)
    {
        $l = User::getPermissions($module);
        if ($l == P_OWNED && $level == P_OWNED && $owner != null)
            return User::getUserID() == $owner;
        return $l >= $level;
    }
    public static function checkError($module, $level, $redirect = null, $owner = null)
    {
        if (Permissions::check($module, $level, $owner))
            return true;

        if ($redirect !== null) {
            Helper::instance()->redirect($redirect);
        } elseif (User::isLogged()) {
            throw new AuthorizationException("Máte nedostatečnou autorizaci pro tuto akci!");
        } else {
            Helper::instance()->redirect('/login?return=' . Request::getURI(),
                'Nemáte dostatečná oprávnění k zobrazení požadovaného obsahu');
        }
    }
}