<?php
namespace TKOlomouc\View;

use TKOlomouc\Utility\Miscellaneous;

class EmptyNotice extends ViewAbstract
{
    private final $filename = 'EmptyNotice.tpl';
    private $header;
    private $notice;

    public function setHeader($header)
    {
        $this->header = $header;
    }

    public function setNotice($notice)
    {
        $this->notice = Miscellaneous\notice($notice);
    }

    public function render()
    {
        $this->renderTemplate(
            $this->filename,
            array(
        	    'header' => $this->header,
                'notice' => $this->notice
            )
        );
    }
}
?>
