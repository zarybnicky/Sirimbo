<?php
namespace TKOlomouc\View;

use TKOlomouc\Utility\Miscellaneous;

class EmptyNotice extends ViewAbstract
{
    private $file = 'EmptyNotice';

    private $header;
    private $notice;

    public function setHeader($header)
    {
        $this->header = $header;
    }

    public function setNotice($notice)
    {
        $this->notice = $notice;
    }

    public function render()
    {
        $this->renderTemplate(
            $this->file,
            array(
                'header' => $this->header,
                'notice' => $this->notice
            )
        );
    }
}
