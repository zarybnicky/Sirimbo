<?php
class DisplayInzerce {
	public static function viewInzerat($data) {
		$member = strpos($_SERVER['REQUEST_URI'], 'member');
		
		echo '<div class="inzerat">';
		
		echo '<div style="float:right;width:250px;text-align:right;">',
			'<span style="color:#572E00;font-size:115%;">Datum přidání: </span>', formatDate($data['i_od']),
			'</div>';
		
		echo '<span class="nadpis">', $data['i_jmeno'], ' ', $data['i_prijmeni'], '</span><br/>';
		
		if(Permissions::canEditInzerat($data['i_reg']))
			echo '<span style="color:#572E00;font-size:115%;">Admin: </span>',
				'<a href="', ($member ? "/member/profil" : ''), '/inzerce/edit/', $data['i_id'],
					'">Editovat</a>', 
				'<a href="', ($member ? "/member/profil" : ''), '/inzerce/remove/', $data['i_id'],
					'">Odstranit</a>';
		
		echo '<div style="clear:both;margin-top:5px;text-align:justify;">', nl2br($data['i_text']), '</div>';
		echo '</div>';
	}
	
	public static function viewInzerce($visible = true, $confirmed = true, $kategorie, $pocet = 0, $vlastnik = 0) {
		$whole = DBInzerce::getInzerce($visible, $confirmed, $kategorie, $pocet, $vlastnik);
		
		if(empty($whole)) {
			return false;
		}
		
		foreach($whole as $key => $row) {
			DisplayInzerce::viewInzerat($row);
			if(count($whole)-1 != $key) {
				echo '<hr/><br/>';
			}
		}
		return true;
	}
}
?>