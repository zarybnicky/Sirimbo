<?php
class DisplayPary {
	public static function viewPartnerRequests($forMe, $byMe) {
		if(!empty($forMe)) {
			foreach($forMe as $request) {
				notice(
					'<form action="/member/profil/par/zadost" method="POST">' .
					'<div style="width:100%;"><span style="float:left;">Uživatel ' .
					$request['u_jmeno'] . ' ' . $request['u_prijmeni'] . ' Vás žádá o partnerství</span>' .
					'<span style="text-align:right;float:right;margin-right:15px;">' .
					'<input type="hidden" name="id" value="' . $request['pn_id'] . '" />' .
					'<button type="submit" name="action" value="accept">Přijmout</button>' .
					'<button type="submit" name="action" value="refuse">Odmítnout</button>' .
					'</span></div></form>');
			}
		}
		if(!empty($byMe)) {
			foreach($byMe as $request) {
				notice(
					'<form action="/member/profil/par/zadost" method="POST">' .
					'<div style="width:100%;"><span style="float:left;">Žádáte uživatele ' .
					$request['u_jmeno'] . ' ' . $request['u_prijmeni'] . ' o partnerství</span>' .
					'<span style="text-align:right;float:right;margin-right:15px;">' .
					'<input type="hidden" name="id" value="' . $request['pn_id'] . '" />' .
					'<button type="submit" name="action" value="cancel">Zrušit</button>' .
					'</span></div></form>');
			}
		}
	}
}