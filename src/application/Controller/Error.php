<?php
namespace TKOlomouc\Controller;

class Error extends ControllerAbstract
{
    function view($id = null) {
        if (!get('id')) get('id', '');

        function ucfirstUser(&$str, $key) {
            $str = ucfirst($str);
        }
        $array = explode('_', get('id'));
        array_walk($array, 'ucfirstUser');
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
    function sidebar() { }
}
?>