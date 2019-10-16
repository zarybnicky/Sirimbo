<?php
abstract class Controller_Abstract implements Controller_Interface
{
    abstract public function view($request);

    public function render($filename, array $vars = [], $standalone = false)
    {
        $renderer = new Renderer();
        $content = $renderer->render($filename, $vars);

        if ($standalone) {
            echo $content;
            return;
        }

        echo $renderer->render(TEMPLATE, [
            'navbar' => $this->getNavbar(),
            'content' => $content,
            'meta' => isset($vars['meta']) ? $vars['meta'] : [],
            'header' => isset($vars['header']) ? $vars['header'] : null,
            'subheader' => isset($vars['subheader']) ? $vars['subheader'] : null,
            'html_title' => isset($vars['html_title']) ? $vars['html_title'] : ''
        ]);
    }

    private function getNavbar()
    {
        $menu = [[
            ['Klub', '/', [
                ['Kluboví trenéři', '/oklubu/treneri/klubovi'],
                ['Externí trenéři', '/oklubu/treneri/externi'],
                ['Kde trénujeme', '/oklubu/saly'],
            ]],
            ['Aktuality', '/aktualne/clanky'],
            ['Videa', '/video'],
            ['Fotogalerie', '/fotogalerie'],
            ['Kontakt', '/kontakt'],
        ]];
        if (Permissions::check('nastenka', P_VIEW)) {
            $menu[] = [
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
                    ['Stránky', '/admin/site', [], ['users', P_ADMIN]],
                ], ['nastenka', P_OWNED]]
            ];
        }
        return $menu;
    }

    public function date()
    {
        return call_user_func_array([new DateHelper(), 'date'], func_get_args());
    }

    public function colorbox()
    {
        return call_user_func_array([new ColorboxHelper(), 'colorbox'], func_get_args());
    }

    public function redirect()
    {
        return call_user_func_array([new RedirectHelper(), 'redirect'], func_get_args());
    }

    public function checkbox()
    {
        return call_user_func_array([new CheckboxHelper(), 'checkbox'], func_get_args());
    }

    public function submit()
    {
        return call_user_func_array([new SubmitHelper(), 'submit'], func_get_args());
    }

    public function radio()
    {
        return call_user_func_array([new RadioHelper(), 'radio'], func_get_args());
    }

    public function person()
    {
        return call_user_func_array([new PersonHelper(), 'person'], func_get_args());
    }

    public function editLink()
    {
        return call_user_func_array([new EditLinkHelper(), 'editLink'], func_get_args());
    }

    public function removeLink()
    {
        return call_user_func_array([new RemoveLinkHelper(), 'removeLink'], func_get_args());
    }

    public function duplicateLink()
    {
        return call_user_func_array([new DuplicateLinkHelper(), 'duplicateLink'], func_get_args());
    }

    public function select()
    {
        return call_user_func_array([new SelectHelper(), 'select'], func_get_args());
    }

    public function notice()
    {
        return call_user_func_array([new NoticeHelper(), 'notice'], func_get_args());
    }

    public function text()
    {
        return call_user_func_array([new TextHelper(), 'text'], func_get_args());
    }

    public function hidden()
    {
        return call_user_func_array([new HiddenHelper(), 'hidden'], func_get_args());
    }

    public function bsRadio()
    {
        return call_user_func_array([new BsRadioHelper(), 'bsRadio'], func_get_args());
    }

    public function userSelect()
    {
        return call_user_func_array([new UserSelectHelper(), 'userSelect'], func_get_args());
    }

    public function __call($name, $args)
    {
        return Helper::invoke($name, $args);
    }
}
