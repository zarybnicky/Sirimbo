<?php
class DisplayInzerce {
	public static function viewInzerat($data) {
		$member = strpos($_SERVER['REQUEST_URI'], 'member');
		
		echo '<div class="inzerat">';
		
		echo '<div style="float:right;width:250px;text-align:right;">',
			'<span style="color:#572E00;font-size:115%;">Datum přidání: </span>', formatDate($data['i_od']),
			'</div>';
		
		if($data['i_reg'] <= 0 && !User::isLogged()) {
			echo '<form action="', $_SERVER['REQUEST_URI'], '" method="POST" style="padding:3px 0">',
				'<span class="nadpis">', $data['i_jmeno'], ' ', $data['i_prijmeni'], '&nbsp;</span>', 
				'<a href="#" id="inz_', $data['i_id'], '" ', 'onclick="$(\'#inz_', $data['i_id'],
					'\').hide().next().show().css(\'display\', \'inline\');">Editovat</a>',
				'<div style="display:none;">',
				'<input type="hidden" name="id" value="', $data['i_id'], '" />',
				'<input type="password" name="pass" style="width:6em;margin:-3px 0;" />',
				'<button type="submit" name="action" value="edit_unreg">Editovat</button>',
				'</div>',
				'</form>';
		} else {
			echo '<span class="nadpis">', $data['i_jmeno'], ' ', $data['i_prijmeni'], '&nbsp;</span>';
		}
		
		if(Permissions::check('inzerce', P_MEMBER) && User::getUserID() == $data['i_reg'])
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