<?php
class Render
{
    public static function twig(string $file, array $vars = []): void
    {
        $twig = new \Twig\Environment(
            new \Twig\Loader\FilesystemLoader('files/Templates'),
            ['cache' => CACHE, 'debug' => true],
        );
        $twig->addExtension(new \Twig\Extension\DebugExtension());
        $twig->addExtension(new RenderTwigExtension());
        echo $twig->render($file, array_merge(self::getGlobals(), $vars));
    }

    public static function getGlobals(): array
    {
        $pos = strpos($_SERVER['REQUEST_URI'], '?');
        $uri = '/' . trim(
            substr(
                $pos !== false ? substr($_SERVER['REQUEST_URI'], 0, $pos) : $_SERVER['REQUEST_URI'],
                strlen(implode('/', array_slice(explode('/', $_SERVER['SCRIPT_NAME']), 0, -1)) . '/')
            ),
            '/'
        );
        return [
            'currentUri' => $uri,
            'currentUser' => \Session::getUser(),
            'navbar' => static::getNavbar(),
        ];
    }

    private static function getNavbar(): array
    {
        $menu = [[
            ['Klub', '/', [
                ['Kluboví trenéři', '/oklubu/klubovi-treneri'],
                ['Externí trenéři', '/oklubu/externi-treneri'],
                ['Kde trénujeme', '/oklubu/saly'],
            ]],
            ['Aktuality', '/aktualne'],
            ['Kontakt', '/kontakt'],
        ]];
        if (\Permissions::check('nastenka', P_VIEW)) {
            $menu[] = [
                ['Nástěnka', '/member', []],
                ['Tréninky', '/member/treninky', []],
                ['Akce', '/member/akce', []],
                ['Dokumenty', '/member/dokumenty', []],
                ['Členové', '/member/clenove', []],
                ['Profil', '/member/profil', []],
                'w-100',
                ['Administrace', '/admin', [
                    ['Uživatelé', '/admin/users', [], ['users', P_OWNED]],
                    ['Skupiny', '/admin/skupiny', [], ['skupiny', P_OWNED]],
                    ['Platby', '/admin/platby', [], ['platby', P_OWNED]],
                    ['Páry', '/admin/pary', [], ['pary', P_OWNED]],
                    ['Články', '/admin/aktuality', [], ['aktuality', P_OWNED]],
                    ['Nástěnka', '/admin/nastenka', [], ['nastenka', P_OWNED]],
                    ['Rozpis', '/admin/rozpis', [], ['rozpis', P_OWNED]],
                    ['Nabídka', '/admin/nabidka', [],['nabidka', P_OWNED]],
                    ['Akce', '/admin/akce', [], ['akce', P_OWNED]],
                    ['Galerie', '/admin/galerie', [], ['galerie', P_OWNED]],
                    ['Dokumenty', '/admin/dokumenty', [], ['dokumenty', P_OWNED]],
                ], ['nastenka', P_OWNED]]
            ];
        }
        return $menu;
    }
}
