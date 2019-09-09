<?php
abstract class Controller_Abstract implements Controller_Interface
{
    abstract public function view($request);

    public function renderNavbarItem($item)
    {
        if (!is_array($item)) {
            return "<div class='$item'></div>";
        }
        $active = $item[1] === ('/' . Database::$request->getURI())
            || (strlen($item[1]) > 1 && strpos('/' . Database::$request->getURI(), $item[1]) === 0);
        $active = $active ? ' active' : '';

        if (!isset($item[2]) || empty($item[2])) {
            return '<li class="nav-item' . $active . '"><a class="nav-link" href="'
                . $item[1] . '">' . $item[0] . '</a></li>';
        }
        $x = '<a class="nav-link dropdown-toggle" data-toggle="dropdown" href="#" role="button" aria-haspopup="true" aria-expanded="false">' . $item[0] . '</a>';
        $x .= '<div class="dropdown-menu">';
        foreach ($item[2] as $sub) {
            if (isset($sub[3]) && $sub[3] && !Permissions::check($sub[3][0], $sub[3][1])) {
                continue;
            }
            $x .= '<a class="dropdown-item" href="' . $sub[1] . '">' . $sub[0] . '</a>';
        }
        $x .= '</div>';
        return '<li class="nav-item' . $active . ' dropdown">' . $x . '</li>';
    }

    public function render($filename, array $vars = [], $standalone = false)
    {
        $renderer = new Renderer();
        $content = $renderer->render($filename, $vars);

        if ($standalone) {
            echo $content;
            return;
        }

        $args = [
            'renderNavbarItem' => [$this, 'renderNavbarItem'],
            'navbar' => [[
                ['Klub', '/', [
                    ['Kluboví trenéři', '/oklubu/treneri/klubovi'],
                    ['Externí trenéři', '/oklubu/treneri/externi'],
                    ['Kde trénujeme', '/oklubu/saly'],
                ]],
                ['Aktuality', '/aktualne/clanky'],
                ['Videa', '/video'],
                ['Fotogalerie', '/fotogalerie'],
                ['Kontakt', '/kontakt'],
            ], [
                ['Nástěnka', '/member/home', []],
                ['Rozpis', '/member/rozpis', []],
                ['Nabídka', '/member/nabidka', []],
                ['Akce', '/member/akce', []],
                ['Dokumenty', '/member/dokumenty', []],
                ['Členové', '/member/clenove/structure', []],
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
                ], ['nastenka', P_OWNED]]
            ]],
            'meta' => [],
            'content' => $content,
            'header' => isset($vars['header']) ? $vars['header'] : null,
            'subheader' => isset($vars['subheader']) ? $vars['subheader'] : null,
            'html_title' => ''
        ];
        if (isset($vars['meta'])) {
            $args['meta'] = array_map(
                function ($k, $v) {
                    return "$k=\"$v\"";
                },
                array_keys($vars['meta']),
                $vars['meta']
            );
        }
        if (isset($vars['html_title'])) {
            $args['html_title'] = $vars['html_title'];
        }

        echo $renderer->render(TEMPLATE, $args);
    }

    public function __call($name, $args)
    {
        return Helper::invoke($name, $args);
    }
}
