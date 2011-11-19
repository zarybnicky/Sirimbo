<?php
class DisplayInzerce {
	public static function viewInzerat($data) {
		echo $data['i_nadpis'], ': ';
		echo '<b>', $data['i_text'], '</b>';
		$member = strpos($_SERVER['REQUEST_URI'], 'member');
		if(Permissions::canEditInzerat($data['i_reg']))
			echo ' - <a href="', ($member ? "/member/profil" : ''), '/inzerce/edit/', $data['i_id'], '">Editovat</a>', 
				'<a href="', ($member ? "/member/profil" : ''), '/inzerce/remove/', $data['i_id'], '">Odstranit</a>';
		echo '<br/><br/>';
	}
	
	public static function viewInzerce($visible = true, $confirmed = true, $kategorie, $pocet = 0, $vlastnik = 0) {
		$whole = DBInzerce::getInzerce($visible, $confirmed, $kategorie, $pocet, $vlastnik);
		
		if(empty($whole)) {
			return false;
		}
		
		foreach($whole as $key => $row) {
			DisplayInzerce::viewInzerat($row);
			if(count($whole)-1 != $key) {
				echo '<hr/>';
			}
		}
		return true;
	}
}
?>