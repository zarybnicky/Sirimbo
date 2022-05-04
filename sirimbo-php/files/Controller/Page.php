<?php
namespace Olymp\Controller;

class Page
{
    public static function get()
    {
        \Render::twig('Main/Page.twig', [
            'page' => \DBPage::getSinglePage($_SERVER['REQUEST_URI'])
        ]);
    }
}
