<?php
class DisplayAktuality {
	public static function viewClanek($data, $preview = false) {
		if($preview)
			echo '<a href="/aktualne/', $data['at_id'],
				'" style="font-size: 115%;color: #572E00;margin: 5px 0 5px 0;">',
				$data['at_jmeno'], '</a><br/>';
		else
			header_minor($data['at_jmeno']);
		
		echo formatTimestamp($data['at_aktu']);
		if(Permissions::check('aktuality', P_OWNED, $data['at_kdo']))
			echo '&nbsp;&nbsp;- <a href="/admin/aktuality/edit/', $data['at_id'], '">Upravit</a>';
		echo '<br/>';
		
		if($preview) {
			echo stripslashes(nl2br($data['at_preview'])),
				'<a href="/aktualne/', $data['at_id'], '">...</a><br/>';
		} else {
			echo stripslashes(nl2br($data['at_text'])), '<br/><br/>';
			echo '<div class="fb-comments" data-href="http://tkolymp.cz/aktualne/' . $data['at_id'] . '" data-width="710"></div>';
		}
	}
	public static function viewAktuality($kat = 0, $kdo = 0) {
		$whole = DBAktuality::getAktuality($kat, $kdo);
		
		if(empty($whole)) {
			return false;
		}
		
		foreach($whole as $key => $row) {
			DisplayAktuality::viewClanek($row, true);
			if(count($whole)-1 != $key) {
				echo '<hr/>';
			}
		}
		return true;
	}
}
?>