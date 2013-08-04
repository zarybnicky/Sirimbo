<?php
class Controller_Registracedone extends Controller_Abstract {
    function view($id = null) {
        header_main("Registrace");
        echo 'Registrace úspěšně proběhla.<br /><br />';
        echo 'Během několika dnů vám na email příjde potvrzení vašeho účtu, které vyřizuje administrátor ručně.'; 
    }
    function sidebar() {
    	
    }
}
?>