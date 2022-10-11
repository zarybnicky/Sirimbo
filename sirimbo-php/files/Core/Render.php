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

        $pos = strpos($_SERVER['REQUEST_URI'], '?');
        $uri = '/' . trim(
            substr(
                $pos !== false ? substr($_SERVER['REQUEST_URI'], 0, $pos) : $_SERVER['REQUEST_URI'],
                strlen(implode('/', array_slice(explode('/', $_SERVER['SCRIPT_NAME']), 0, -1)) . '/')
            ),
            '/'
        );

        echo $twig->render($file, array_merge(['currentUri' => $uri], $vars));
    }
}
