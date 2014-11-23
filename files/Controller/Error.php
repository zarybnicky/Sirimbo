<?php
class Controller_Error extends Controller_Abstract
{
    public function view($request) {
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
            'files/View/Empty.inc',
            array(
                'nadpis' => 'Chyba',
                'notice' => $notice
            )
        );
    }
}
