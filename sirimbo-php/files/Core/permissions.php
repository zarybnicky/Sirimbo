<?php
class Permissions
{
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
        if (\Permissions::check($module, $level, $owner)) {
            return true;
        }

        if (Session::isLogged()) {
            throw new AuthorizationException("Nemáte dostatečnou autorizaci pro tuto akci!");
        }
        new RedirectHelper(
            '/login?return=' . $_SERVER['REQUEST_URI'],
            'Nemáte dostatečná oprávnění k zobrazení požadovaného obsahu'
        );
    }
}
