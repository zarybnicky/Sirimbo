<?php
namespace Olymp\Controller;

class Page
{
    public static function get()
    {
        $url = parse_url($_SERVER['REQUEST_URI'], PHP_URL_PATH);
        \Render::twig('Main/Page.twig', [
            'page' => \DBPage::getSinglePage($url)
        ]);
    }
}
