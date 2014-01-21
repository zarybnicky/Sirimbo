<?php
namespace TKOlomouc\View;

use TKOlomouc\View\Exception\ViewException;

class StaticPage extends Layout
{
    private $file = null;

    public function setFile($file)
    {
        $this->file = $file;
    }

    public function render()
    {
        if ($this->file === null) {
            throw new ViewException('A valid template file is required.');
        }

        $content = $this->renderTemplate($this->file, array());
        $this->set('content', $content);
        return $this->renderLayout();
    }
}