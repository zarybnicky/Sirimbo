<?php
class DisplayPary {
	public static function viewPartnerRequests($forMe, $byMe = array()) {
		if(!empty($forMe)) {
			foreach($forMe as $request) {
				notice(
					'<form action="/member/profil" method="POST">' .
					'<div style="float:left;">Uživatel ' . $request['u_jmeno'] . ' ' .
					$request['u_prijmeni'] . ' Vás žádá o partnerství</div>' .
					'<div style="float:right;margin-right: 15px;">' .
					'<input type="hidden" name="id" value="' . $request['pn_id'] . '" />' .
					'<button type="submit" name="action" value="mate-accept">Přijmout</button>' .
					'<button type="submit" name="action" value="mate-refuse">Odmítnout</button>' .
					'</div></form>');
			}
		}
		if(!empty($byMe)) {
			foreach($byMe as $request) {
				notice(
					'<form action="/member/profil" method="POST">' .
					'<div style="float:left;">Žádáte uživatele ' .
					$request['u_jmeno'] . ' ' . $request['u_prijmeni'] . ' o partnerství</div>' .
					'<div style="float:right;margin-right: 15px;">' .
					'<input type="hidden" name="id" value="' . $request['pn_id'] . '" />' .
					'<button type="submit" name="action" value="mate-cancel">Zrušit</button>' .
					'</div></form>');
			}
		}
	}
}