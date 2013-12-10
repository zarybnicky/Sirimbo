<?php
namespace TKOlomouc\Utility;

use TKOlomouc\View\Exception\AuthorizationException;

class Permissions
{
    const P_NONE = 1;
    const P_VIEW = 2;
    const P_MEMBER = 4;
    const P_OWNED = 8;
    const P_ADMIN = 16;

    public static function get($module)
    {
        return User::getPermissions($module);
    }

    public static function check($module, $level, $vlastnik = null)
    {
        $perms = User::getPermissions($module);
        if ($perms == self::P_OWNED && $level == self::P_OWNED && $vlastnik != null) {
            return User::getUserID() == $vlastnik;
        }
        return $perms >= $level;
    }

    public static function checkError($module, $level, $redirect = null, $vlastnik = null)
    {
        if (self::check($module, $level, $vlastnik)) {
            return true;
        }
        if ($redirect !== null) {
            Response::redirect($redirect);
            return;
        } elseif (User::isLogged()) {
            throw new AuthorizationException("Máte nedostatečnou autorizaci pro tuto akci!");
        } else {
            Response::redirect(
                '/login?return=' . Request::getURL(),
                'Nemáte dostatečná oprávnění k zobrazení požadovaného obsahu'
            );
        }
    }
}
