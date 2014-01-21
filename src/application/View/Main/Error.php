<?php
namespace TKOlomouc\View\Main;

use TKOlomouc\Utility\Response;
use TKOlomouc\Utility\Miscellaneous;
use TKOlomouc\View\ViewAbstract;
use TKOlomouc\View\Layout;

class Error extends Layout
{
    private $errorId = '';

    public function setErrorId($id)
    {
        $this->errorId = $id;
    }

    public function render()
    {
        $notice = $this->renderTemplate('Error/' . $this->errorId);
        if(!$notice) {
            $notice = 'Chybová stránka s daným ID nebyla nalezena';
        }

        $content = $this->renderTemplate(
            'EmptyNotice',
            array(
                'header' => 'Chyba',
                'notice' => $notice
            )
        );
        
        $this->set('content', $content);
        return $this->renderLayout();
    }
}
