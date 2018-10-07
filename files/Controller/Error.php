<?php
class Controller_Error extends Controller_Abstract
{
    public function view($request)
    {
        function ucfirstUser(&$str, $key)
        {
            $str = ucfirst($str);
        }
        $array = explode('_', $request->get('id'));
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
        $this->redirect()->setMessage($notice);
        $this->render('files/View/Empty.inc', ['header' => 'Chyba']);
    }
}
