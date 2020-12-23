<?php
class RenderHelper
{
    public function __construct($filename, array $vars = [], $standalone = false)
    {
        $pos = strpos($_SERVER['REQUEST_URI'], '?');
        $uri = '/' . trim(
            substr(
                $pos !== false ? substr($_SERVER['REQUEST_URI'], 0, $pos) : $_SERVER['REQUEST_URI'],
                strlen(implode('/', array_slice(explode('/', $_SERVER['SCRIPT_NAME']), 0, -1)) . '/')
            ),
            '/'
        );
        $globals = [
            'currentUri' => $uri,
            'currentUser' => \Session::getUser(),
        ];

        $renderer = new \Renderer();
        $content = $renderer->render($filename, array_merge($globals, $vars));

        if ($standalone) {
            echo $content;
            return;
        }

        echo $renderer->render('files/Template.inc', array_merge($globals, [
            'navbar' => static::getNavbar(),
            'content' => $content,
            'meta' => $vars['meta'] ?? [],
            'header' => $vars['header'] ?? null,
            'subheader' => $vars['subheader'] ?? null,
            'html_title' => $vars['html_title'] ?? '',
        ]));
    }

    private static function getNavbar()
    {
        $menu = [[
            ['Klub', '/', [
                ['Kluboví trenéři', '/oklubu/klubovi-treneri'],
                ['Externí trenéři', '/oklubu/externi-treneri'],
                ['Kde trénujeme', '/oklubu/saly'],
            ]],
            ['Aktuality', '/aktualne'],
            ['Videa', '/video'],
            ['Fotogalerie', '/fotogalerie'],
            ['Kontakt', '/kontakt'],
        ]];
        if (\Permissions::check('nastenka', P_VIEW)) {
            $menu[] = [
                ['Nástěnka', '/member', []],
                ['Rozpis', '/member/rozpis', []],
                ['Nabídka', '/member/nabidka', []],
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
                    ['Video', '/admin/video', [], ['aktuality', P_OWNED]],
                    ['Dokumenty', '/admin/dokumenty', [], ['dokumenty', P_OWNED]],
                    ['Oprávnění', '/admin/permissions', [], ['permissions', P_OWNED]],
                    ['Stránky', '/admin/site', [], ['users', P_ADMIN]],
                ], ['nastenka', P_OWNED]]
            ];
        }
        return $menu;
    }
}
