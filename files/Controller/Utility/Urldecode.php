<?php
class Controller_Utility_Urldecode extends Controller_Abstract
{
    public function view($request) {
        echo '<form action="" method="GET">';
        echo '<h1>URL Decode</h1>';
        echo 'Text pro urldecode(): <input type="text" name="t" /><br/>';
        echo $this->submit('Odeslat');
        echo '</form>';

        if ($request->get('t')) {
            echo urldecode($request->get('t'));
        }
    }
}
