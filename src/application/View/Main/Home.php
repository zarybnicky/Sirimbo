<?php
namespace TKOlomouc\View\Main;

use TKOlomouc\Utility\Response;
use TKOlomouc\Utility\Miscellaneous;
use TKOlomouc\View\ViewAbstract;
use TKOlomouc\View\Helper\Zpravy;
use TKOlomouc\View\Helper\Clanky;
use TKOlomouc\Utility\Debug;
use TKOlomouc\View\Layout;

class Home extends Layout
{
    private $file = 'Main/Home';
    private $clanky = array();
    private $zpravy = array();

    public function setClanky(array $data)
    {
        $this->clanky = $data;
    }

    public function setZpravy(array $data)
    {
        $this->zpravy = $data;
    }

    public function render()
    {
        $notice = Miscellaneous::notice(Response::getMessage());

        $slideshow = new Clanky();
        $slideshow->populate($this->clanky);

        $clankyList = clone $slideshow;

        $slideshow
            ->count(5)
            ->setSlideshow(true);
        $slideshow = $slideshow->render();

        $clankyList
            ->count(9)
            ->offset(5)
            ->setSlideshow(false);
        $clankyList = $clankyList->render();

        $zpravy = new Zpravy();
        $zpravy
            ->setCount(20)
            ->populate($this->zpravy);
        $zpravy = $zpravy->render();

        $content = $this->renderTemplate(
            $this->file,
            array(
                'notice' => $notice,
                'clankySlideshow' => $slideshow,
                'clankyList' => $clankyList,
                'zpravy' => $zpravy
            )
        );

        $this->set('content', $content);
        return $this->renderLayout();
    }
}
