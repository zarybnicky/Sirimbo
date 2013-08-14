<?php
include_once('files/Controller/Member.php');
class Controller_Member_Clenove extends Controller_Member {
	function __construct() {
		Permissions::checkError('users', P_VIEW);
	}
	function view($id = null) {
		if($id && ($data = DBUser::getUserData($id))) {
			header_main("Členové");
			echo '<b>', $data['u_jmeno'], ' ', $data['u_prijmeni'], '</b><br/>';
			echo 'E-mail: ', $data['u_email'], '<br/>';
			echo 'Telefon: ', $data['u_telefon'], '<br/>';
			echo 'Poznámky: ', $data['u_poznamky'], '<br/>';
			echo 'Variabilní symbol: ', User::var_symbol($id), '<br/>';
			echo '<a href="', Request::getReferer(), '">Zpět</a>';
			return;
		}
		
		include('files/Member/Clenove.inc');
	}
}
?>