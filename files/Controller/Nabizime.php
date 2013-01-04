<?php
class Controller_Nabizime implements Controller_Interface {
    function view($id = null) {
        View::redirect('/nabizime/obecne');
    }
    function obecne($id = null) {
        include('files/Main/Nabizime/Main.inc');
    }
    function vystoupeni($id = null) {
        include('files/Main/Nabizime/Vystoupeni.inc');
    }
    function individualky($id = null) {
        include('files/Main/Nabizime/Individualky.inc');
    }
    function seminare($id = null) {
        include('files/Main/Nabizime/Seminare.inc');
    }
    function soustredeni($id = null) {
        include('files/Main/Nabizime/Seminare.inc');
    }
}
?>