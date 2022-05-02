<?php
namespace Olymp\Controller;

class Nabor
{
    public static function get()
    {
        \Render::twig('Main/Nabor.twig', [
            'page' => \DBPage::getSinglePage('/prijdtancit')
        ]);
    }
}
