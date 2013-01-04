<?php
class Controller_Registracedone implements Controller_Interface {
    function view($id = null) {
        header_main("Registrace");
        echo 'Registrace úspěšně proběhla.<br /><br />';
        echo 'Během několika dnů vám na email příjde potvrzení vašeho účtu, které vyřizuje administrátor ručně.'; 
    }
}
?>