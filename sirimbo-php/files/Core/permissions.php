<?php
class Permissions
{
    public static function getForModule(string $module): int
    {
        $user = \Session::getUser();
        if (!$user) {
            return P_NONE;
        }
        if ($user->getId() == 1) {
            return P_ADMIN;
        }
        if ($user->getPermissionGroup() == 0) {
            return P_NONE;
        }

        $permissionsRaw = \DBPermissions::getSingleGroup($user->getPermissionGroup());
        $permissions = [];
        foreach (array_keys(self::$permissions) as $key) {
            if ($user->getPermissionGroup() == 0) {
                $permissions[$key] = P_NONE;
            } else {
                $permissions[$key] = $permissionsRaw['pe_' . $key];
            }
        }
        return $permissions[$module] ?? P_NONE;
    }

    public static function check(string $module, int $level, $owner = null): bool
    {
        $l = self::getForModule($module);
        if ($l == P_OWNED && $level == P_OWNED && $owner != null) {
            return \Session::getUser()->getId() == $owner;
        }
        return $l >= $level;
    }

    public static function checkError(string $module, int $level, $owner = null): void
    {
        if (\Permissions::check($module, $level, $owner)) {
            return;
        }
        if (\Session::getUser()) {
            throw new AuthorizationException("Nemáte dostatečnou autorizaci pro tuto akci!");
        }
        \Message::warning('Nemáte dostatečná oprávnění k zobrazení požadovaného obsahu');
        \Redirect::to('/login?return=' . $_SERVER['REQUEST_URI']);
    }
}
