<?php
namespace TKOlomouc\View;

use MtHaml;
use TKOlomouc\Utility\Debug;
abstract class ViewAbstract
{
    abstract function render();

    protected function renderTemplate($filename, array $context)
    {
        $mtHaml = new MtHaml\Environment('php');

        try {
            return $mtHaml->compileString(file_get_contents($filename), $filename);
        } catch(Exception $e) {
            Debug::dump($e);
        }
    }
}
