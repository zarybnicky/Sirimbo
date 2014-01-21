<?php
namespace TKOlomouc\View;

use TKOlomouc\View\ViewAbstract;
use TKOlomouc\Utility\Request;
use TKOlomouc\Utility\Debug;

abstract class Layout extends ViewAbstract
{
    private $request;

    protected $section = array(
        'head'       => array(),
        'css'        => array(),
        'javascript' => array(),
        'title'      => array(),
        'season'     => array(),
        'userbox'    => array(),
        'topmenu'    => array(),
        'sidemenu'   => array(),
        'toolbox'    => array(),
        'content'    => array()
    );
    protected $layout;

    public function __construct(Request $request, $layout)
    {
        $this->request = $request;
        $this->layout = $layout;

        $date = (int) date("md");
        if ($date < 320) {
            $this->section['season'] = 'zima';
        } elseif ($date < 621) {
            $this->section['season'] = 'jaro';
        } elseif ($date < 922) {
            $this->section['season'] = 'leto';
        } else {
            $this->section['season'] = 'podzim';
        }

        $sections = array(
            'home',
            'oklubu',
            'aktualne',
            'fotogalerie',
            'nabizime',
            'member',
            'kontakt'
        );

        $this->section['visible'] = array_flip($sections);
        $found = false;

        foreach ($sections as $item) {
            if ($this->request->getSection() != $item) {
                $this->section['visible'][$item] = '';
                continue;
            }
            $this->section['visible'][$item] = 'current';
            $found = true;
        }
        if (!$found) {
            $this->section['visible']['home'] = 'current';
        }
    }

    public function set($name, $content, $prepend = false, $overwrite = false)
    {
        if ($overwrite == true || !isset($this->section[$name])) {
            $this->section[$name] = array();
        }
        if ($prepend == true) {
            array_unshift($this->section[$name], $content);
            return;
        }
        array_push($this->section[$name], $content);
    }

    protected function renderLayout(array $context = array())
    {
        foreach ($this->section as $name => &$content) {
            if (!is_array($content)) {
                continue;
            }

            //If $content is an associative array (has at least one string key)
            if (count(array_filter(array_keys($content), 'is_string')) > 0) {
                continue;
            }

            ksort($content);
            $content = implode("\n", $content);
        }

        $context += $this->section;

        return $this->renderTemplate('Layout/' . ucfirst($this->layout), $context);
    }
}
