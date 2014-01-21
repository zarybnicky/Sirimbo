<?php
namespace TKOlomouc\View;

use MtHaml;
use TKOlomouc\Utility\Debug;
use TKOlomouc\Utility\Renderer;
use TKOlomouc\View\Exception\NotFoundRightException;

abstract class ViewAbstract
{
    protected $extensions = array(
        'haml' => array(
            '.haml',
            '.tpl'
        ),
        'php' => array(
            '.inc',
            '.php'
        )
    );

    protected $directories = array(
        '.',
        'src/application/Template',
        'src/library/Template'
    );

    public function addTemplateDirectory($directory)
    {
        $this->directories[] = $directory;
    }

    protected function renderTemplate($template, array $context = array())
    {
        $filename = $this->resolveName($template);

        if ($filename === null) {
            throw new NotFoundRightException("Template '$template' not found");
        }

        $renderer = new Renderer();

        try {
            return $renderer->render($filename, $context);
        } catch (\Exception $e) {
            throw $e;
        }
    }

    protected function prepareHaml($template)
    {
        $compiled = $template . '.php';

        if (!file_exists($compiled) || filemtime($compiled) != filemtime($template)) {
            $mtHaml = new MtHaml\Environment('php');

            $hamlCode = file_get_contents($template);
            $phpCode = $mtHaml->compileString($hamlCode, $template);

            $tempnam = tempnam(dirname($template), basename($template));
            file_put_contents($tempnam, $phpCode);
            rename($tempnam, $compiled);

            touch($compiled, filemtime($template));
        }
        return $compiled;
    }

    /**
     * Tries to find a template in directories $this->directories with extensions
     * $this->extensions.
     *
     * @todo Handle HAML separately
     * @todo Add CSS handling?
     * @param string $filename
     * @return string|NULL Returns null if file was not found
     */
    protected function resolveName($filename)
    {
        foreach ($this->extensions as $type => $extensions) {
            foreach ($extensions as $extension) {
                foreach ($this->directories as $directory) {
                    $file = $directory . DIRECTORY_SEPARATOR . $filename . $extension;

                    if (!file_exists($file)) {
                        continue;
                    }
                    if ($type == 'haml') {
                        $file = $this->prepareHaml($file);
                    }
                    return $file;
                }
            }
        }
        return null;
    }

    abstract public function render();
}
