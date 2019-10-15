<?php
class Controller_Error extends Controller_Abstract
{
    public function view($request)
    {
        $array = explode('_', $request->get('id'));
        array_walk($array, function (&$str) {
            $str = ucfirst($str);
        });
        $id = implode('', $array);
        $file = ERROR . DIRECTORY_SEPARATOR . $id . '.inc';

        if (file_exists($file)) {
            ob_start();
            include $file;
            $notice = ob_get_clean();
        } else {
            $notice = "Chybová stránka s daným ID nebyla nalezena";
        }
        $this->redirect()->danger($notice);
        $this->render('files/View/Empty.inc', ['header' => 'Chyba']);
    }
}
