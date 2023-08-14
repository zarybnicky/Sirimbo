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
            'frontendHash' => defined('FRONTEND_HASH') ? constant('FRONTEND_HASH') : '8',
            'currentUser' => \Session::getUser(),
            'navbar' => self::getNavbar(),
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
            ['Aktuality', '/clanky'],
            ['Akce', '/akce'],
            ['Kontakt', '/kontakt'],
        ]];
        if (\Permissions::check('nastenka', P_VIEW)) {
            $menu[] = [
                ['Nástěnka', '/member', []],
                ['Tréninky', '/member/treninky', []],
                ['Akce', '/akce', []],
                ['Dokumenty', '/member/dokumenty', []],
                ['Členové', '/member/clenove', []],
                ['Páry', '/pary', []],
                ['Profil', '/member/profil', []],
                'w-100',
                ['Administrace', '/', [
                    ['Uživatelé', '/users', [], ['users', P_OWNED]],
                    ['Skupiny', '/skupiny', [], ['skupiny', P_OWNED]],
                    ['Platby', '/platby', [], ['platby', P_OWNED]],
                    ['Články', '/aktuality', [], ['aktuality', P_OWNED]],
                    ['Nástěnka', '/nastenka', [], ['nastenka', P_OWNED]],
                    ['Galerie', '/galerie', [], ['galerie', P_OWNED]],
                    ['Dokumenty', '/dokumenty', [], ['dokumenty', P_OWNED]],
                ], ['nastenka', P_OWNED]]
            ];
        }
        return $menu;
    }
}
