<?php
namespace TKOlomouc\Utility;

use TKOlomouc\View\Exception\AuthorizationException;

class Permissions
{
    public static function get($module)
    {
        return User::getPermissions($module);
    }
    public static function check($module, $level, $vlastnik = null)
    {
        $perms = User::getPermissions($module);
        if ($perms == P_OWNED && $level == P_OWNED && $vlastnik != null) {
            return User::getUserID() == $vlastnik;
        }
        return $perms >= $level;
    }
    public static function checkError($module, $level, $redirect = null, $vlastnik = null)
    {
        if (Permissions::check($module, $level, $vlastnik))
            return true;

        if ($redirect !== null) {
            Response::redirect($redirect);
        } elseif (User::isLogged()) {
            throw new AuthorizationException("Máte nedostatečnou autorizaci pro tuto akci!");
        } else {
            Response::redirect('/login?return=' . Request::getURL(),
                'Nemáte dostatečná oprávnění k zobrazení požadovaného obsahu');
        }
    }
}