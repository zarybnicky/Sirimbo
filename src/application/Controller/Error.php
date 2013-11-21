<?php
namespace TKOlomouc\Controller;

class Error extends ControllerAbstract
{
    public function view($id = null)
    {
        if (!get('id')) {
            get('id', '');
        }
        $array = explode('_', get('id'));
        array_walk(
            $array,
            function (&$str, $key) {
                $str = ucfirst($str);
            }
        );
        $id = implode('', $array);
        $file = ERROR . DIRECTORY_SEPARATOR . $id . '.inc';

        if (file_exists($file)) {
            ob_start();
            include $file;
            $notice = ob_get_clean();
        } else {
            $notice = "Chybová stránka s daným ID nebyla nalezena";
        }

        $this->render(
            'src/application/View/Empty.inc',
            array(
                'nadpis' => 'Chyba',
                'notice' => $notice
            )
        );
    }

    public function sidebar()
    {
        ;
    }
}
