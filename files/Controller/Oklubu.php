<?php
class COntroller_Oklubu implements Controller_Interface {
    function view($id = null) {
       View::redirect('/oklubu/obecne');
    }
    function obecne($id = null) {
        include('files/Main/OKlubu/Main.inc');
    }
    function historie($id = null) {
        include('files/Main/OKlubu/Historie.inc');
    }
    function uspechy($id = null) {
        include('files/Main/OKlubu/VCislech.inc');
    }
    function mistrovstvi($id = null) {
        include('files/Main/OKlubu/Mistrovstvi.inc');
    }
    function druzstva($id = null) {
        include('files/Main/OKlubu/Druzstva.inc');
    }
    function liga($id = null) {
        include('files/Main/OKlubu/Liga.inc');
    }
    function klubovi($id = null) {
        include('files/Main/OKlubu/TreneriInt.inc');
    }
    function externi($id = null) {
        include('files/Main/OKlubu/TreneriExt.inc');
    }
    function saly($id = null) {
        include('files/Main/OKlubu/Saly.inc');
    }
}
?>