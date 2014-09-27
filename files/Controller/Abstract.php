<?php
abstract class Controller_Abstract implements Controller_Interface
{
    abstract public function view($id = null);

    public function sidebar() {
        echo new Navbar(
            array(
                array(
                    'Domů',
                    '/home',
                    array()
                ),
                array(
                    'O klubu',
                    '/oklubu',
                    array(
                        array(
                            'Historie',
                            '/oklubu/historie',
                            array()
                        ),
                        array(
                            'Úspěchy v číslech',
                            '/oklubu/uspechy',
                            array()
                        ),
                        array(
                            'Mistrovství ČR',
                            '/oklubu/mistrovstvi',
                            array()
                        ),
                        array(
                            'Mistrovství družstev',
                            '/oklubu/druzstva',
                            array()
                        ),
                        array(
                            'Taneční liga',
                            '/oklubu/liga',
                            array()
                        ),
                        array(
                            'Externí trenéři',
                            '/oklubu/treneri/externi',
                            array()
                        ),
                        array(
                            'Kluboví trenéři',
                            '/oklubu/treneri/',
                            array()
                        ),
                        array(
                            'Kde trénujeme',
                            '/oklubu/saly',
                            array()
                        ),
                        array(
                            'Stanovy klubu',
                            '/oklubu/stanovy.pdf',
                            array()
                        )
                    )
                ),
                array(
                    'Novinky',
                    '/aktualne/clanky',
                    array()
                ),
                array(
                    'Nabízíme',
                    '/nabizime',
                    array(
                        array(
                            'Taneční kurzy',
                            '/nabizime/obecne',
                            array()
                        ),
                        array(
                            'Taneční vystoupení',
                            '/nabizime/vystoupeni',
                            array()
                        ),
                        array(
                            'Taneční soustředění',
                            '/nabizime/soustredeni',
                            array()
                        ),
                        array(
                            'Tancování pro děti',
                            '/nabizime/seminare',
                            array()
                        ),
                        array(
                            'Zázemí pro taneční sport',
                            '/nabizime/individualky',
                            array()
                        )
                    )
                ),
                array(
                    'Fotogalerie',
                    '/fotogalerie',
                    array()
                ),
                array(
                    'Kontakt',
                    '/kontakt',
                    array()
                )
            ),
            true,
            array()
        );

        $parent = get_parent_class($this);
        if ($parent && $parent != 'Controller_Abstract' && method_exists($parent, "sidebar")) {
            return call_user_method('sidebar', new $parent);
        }
        return '';
    }

    public function render($filename, array $vars = array(), $standalone = false) {
        ob_start();
        $r = new Renderer();
        $content = $r->render($filename, $vars);

        if ($standalone) {
            echo $content;
            return;
        }
        include TISK ? HEADER_TISK : HEADER;
        echo $content;
        include TISK ? FOOTER_TISK : FOOTER;
    }
    public function __call($name, $args) {
        $trace = debug_backtrace();
        $class = (isset($trace[1]['class']) ? $trace[1]['class'] : null);

        if (is_subclass_of($class, __CLASS__)) {
            if (empty($args))
                return Helper::get()->$name();
            else
                return call_user_func_array(array(Helper::get(), $name), $args);
        } else {
            throw new ViewException("Neplatná akce $name pro ${__CLASS__}");
        }
    }
}