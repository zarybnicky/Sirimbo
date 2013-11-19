<?php
namespace TKOlomouc\View;

class RemovePrompt extends ViewAbstract
{
    private $filename = '';
    private $header;
    private $prompt;
    private $data = array();
    private $returnURL;

    public function setHeader($header)
    {
        $this->header = $header;
    }

    public function setPrompt($prompt)
    {
        $this->prompt = $prompt;
    }

    public function addData($id, $text)
    {
        $this->data[] = array(
            'id'   => $id,
            'text' => $text
        );
    }

    public function setReturnURL($url)
    {
        $this->returnURL = $url;
    }

    function render()
    {
        $this->renderTemplate(
            $this->filename,
            array(
        	    'header'    => $this->header,
                'prompt'    => $this->prompt,
                'data'      => $this->data,
                'returnURL' => $this->returnURL
            )
        );
    }
}
