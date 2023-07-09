<?php
class Permissions
{
    public static $permissionCache = [];

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

        $id = $user->getPermissionGroup();
        if (!isset(self::$permissionCache[$id])) {
            self::$permissionCache[$id] = \Database::querySingle("SELECT * FROM permissions WHERE pe_id='?'", $id);
        }
        $permissionsRaw = self::$permissionCache[$id];
        $permissions = [];
        foreach (array_keys(self::$permissions) as $key) {
            $permissions[$key] = $permissionsRaw['pe_' . $key];
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

    public static $permissions = [
        'akce' => [
            'name' => "Akce",
            'default' => P_MEMBER,
            P_NONE => 1, P_VIEW => 1, P_MEMBER => 1, P_OWNED => 1, P_ADMIN => 1],
        'aktuality' => [
            'name' => "Aktuality",
            'default' => P_VIEW,
            P_VIEW => 1, P_OWNED => 1, P_ADMIN => 1],
        'dokumenty' => [
            'name' => "Dokumenty",
            'default' => P_MEMBER,
            P_NONE => 1, P_MEMBER => 1, P_OWNED => 1, P_ADMIN => 1],
        'galerie' => [
            'name' => "Fotogalerie",
            'default' => P_VIEW,
            P_VIEW => 1, P_OWNED => 1, P_ADMIN => 1],
        'nabidka' => [
            'name' => "Nabídka",
            'default' => P_MEMBER,
            P_NONE => 1, P_VIEW => 1, P_MEMBER => 1, P_OWNED => 1, P_ADMIN => 1],
        'nastenka' => [
            'name' => "Nástěnka",
            'default' => P_VIEW,
            P_NONE => 1, P_VIEW => 1, P_OWNED => 1, P_ADMIN => 1],
        'novinky' => [
            'name' => "Články",
            'default' => P_VIEW,
            P_NONE => 1, P_VIEW => 1, P_OWNED => 1, P_ADMIN => 1],
        'pary' => [
            'name' => "Páry",
            'default' => P_VIEW,
            P_NONE => 1, P_VIEW => 1, P_ADMIN => 1],
        'platby' => [
            'name' => "Platby",
            'default' => P_NONE,
            P_NONE => 1, P_ADMIN => 1],
        'permissions' => [
            'name' => "Oprávnění",
            'default' => P_NONE,
            P_NONE => 1, P_ADMIN => 1],
        'rozpis' => [
            'name' => "Rozpis",
            'default' => P_MEMBER,
            P_NONE => 1, P_VIEW => 1, P_MEMBER => 1, P_OWNED => 1, P_ADMIN => 1],
        'skupiny' => [
            'name' => "Skupiny",
            'default' => P_VIEW,
            P_NONE => 1, P_VIEW => 1, P_ADMIN => 1],
        'users' => [
            'name' => "Uživatelé",
            'default' => P_VIEW,
            P_NONE => 1, P_VIEW => 1, P_OWNED => 1, P_ADMIN => 1],
        'main' => [
            'name' => "Veřejná část",
            'default' => P_VIEW,
            P_VIEW => 1]
    ];
}
