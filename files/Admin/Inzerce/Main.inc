<?php
User::checkPermissionsError(L_ADMIN);
header_main("Správa inzerce");

notice(View::getRedirectMessage());

switch(Request::getAction()) {
	case 'add':
		if(empty($_POST)) {
			include('files/Admin/Inzerce/Form.inc');
			return;
		} else {
			$od = post('od_year') . '-' . post('od_month') . '-' . post('od_day');
			$do = post('do_year') . '-' . post('do_month') . '-' . post('do_day');
			
			if($do == '0000-00-00' || strcmp($od, $do) > 0)
				$do = $od;
		
			$userid = post('reg');
			$user_data = DBUser::getUserData(post('reg'));
			$jmeno = $user_data['u_jmeno'];
			$prijmeni = $user_data['u_prijmeni'];
			
			$confirmed = '1';
			$visible = (bool) post('visible');
			
			$error = false;
			
			ob_start();
			include('files/Admin/Inzerce/Form.inc');	
			$html = ob_get_clean();
			
			if(!$error) {
				DBInzerce::addInzerat(post('kat'), $userid, $jmeno, $prijmeni,
					post('nadpis'), post('text'), serialize(array()), $od, $do,
					'', $visible, $confirmed);
				View::redirect('/admin/inzerce', 'Inzerát přidán');
			} else {
				echo $html;
				return;
			}
		}
		break;
	
	case 'edit':
		$id = Request::getID();
		if(!$id || !is_numeric($id) || !($data = DBInzerce::getSingleInzerat($id)))
			View::redirect('/admin/inzerce', 'Inzerát s takovým ID neexistuje');
		
		if(empty($_POST)) {
			post('kat', $data['i_kat']);
			post('reg', $data['i_reg']);
			post('jmeno', $data['i_jmeno']);
			post('prijmeni', $data['i_prijmeni']);
			post('nadpis', $data['i_nadpis']);
			post('text', $data['i_text']);
			list($od_year, $od_month, $od_day) = explode('-', $data['i_od']);
			post('od_year', $od_year);
			post('od_month', $od_month);
			post('od_day', $od_day);
			list($do_year, $do_month, $do_day) = explode('-', $data['i_do']);
			post('do_year', $do_year);
			post('do_month', $do_month);
			post('do_day', $do_day);
			post('pass', $data['i_pass']);
			post('visible', $data['i_visible']);
			post('confirmed', $data['i_confirmed']);
			
			include('files/Admin/Inzerce/Form.inc');
			return;
		} else {
			$od = post('od_year') . '-' . post('od_month') . '-' . post('od_day');
			$do = post('do_year') . '-' . post('do_month') . '-' . post('do_day');
			
			if($do == '0000-00-00' || strcmp($od, $do) > 0)
				$do = $od;
			
			$userid = post('reg');
			if($userid > 0) {
				$user_data = DBUser::getUserData(post('reg'));
				$jmeno = $user_data['u_jmeno'];
				$prijmeni = $user_data['u_prijmeni'];
			} else {
				$jmeno = post('jmeno');
				$prijmeni = post('prijmeni');
			}
			$confirmed = '1';
			$visible = (bool) post('visible');
			
			$error = false;
			
			ob_start();
			include('files/Admin/Inzerce/Form.inc');	
			$html = ob_get_clean();
			
			if(!$error) {
				DBInzerce::editInzerat($id, post('kat'), $userid, $jmeno, $prijmeni,
					post('nadpis'), post('text'), serialize(array()), $od, $do,
					$data['i_pass'], $visible, $confirmed);
				View::redirect("/admin/inzerce", "Inzerát upraven");
			} else {
				echo $html;
				return;
			}
		}
		break;
	case 'new':
		if(empty($_POST)) {
			include('files/Admin/Inzerce/DisplayNew.inc');
			return;
		} else {
			if(post('action') == 'confirm') {
				if(is_array(post('inzerce'))) {
					foreach(post('inzerce') as $id) {
						$visible = post($id . '-visible');
						DBInzerce::confirmInzerat($id, (bool) $visible);
					}
				}
				View::redirect('/admin/inzerce', 'Inzeráty potvrzeny');
				return;
			} elseif(post('action') == 'remove') {
				echo '<form action="/admin/inzerce" method="POST">';
				echo 'Opravdu chcete odstranit inzeráty:<br/><br/>';
				foreach(post('inzerce') as $id) {
					$data = DBInzerce::getSingleInzerat($id);
					echo "\t", $data['i_nadpis'];
					echo '<br />';
					echo '<input type="hidden" name=inzerce[]" value="', $id, '" />';
				}
				echo '<br/>';
				echo '<button type="submit" name="action" value="remove_confirm">Odstranit</button>';
				echo '</form>';
				echo '<a href="/admin/inzerce">Zpět</a>';
				return;
			}
		}
		break;
}

if(empty($_POST)) {
	include("files/Admin/Inzerce/Display.inc");
	return;
}

switch(post("action")) {
	case 'edit':
		$inzerce = post('inzerce');
		if($inzerce[0]) {
			header('Location: /admin/inzerce/edit/' . $inzerce[0]);
			return;
		}
		break;

	case 'remove':
		if(is_array(post('inzerce'))) {
			echo '<form action="/admin/inzerce" method="post">';
			echo 'Opravdu chcete odstranit inzeráty:<br/><br/>';
			foreach(post('inzerce') as $id) {
				$data = DBInzerce::getSingleInzerat($id);
				echo "\t", $data['i_nadpis'];
				echo '<br />';
				echo '<input type="hidden" name=inzerce[]" value="', $id, '" />';
			}
			echo '<br/>';
			echo '<button type="submit" name="action" value="remove_confirm">Odstranit</button>';
			echo '</form>';
			echo '<a href="/admin/inzerce">Zpět</a>';
			return;
		}
		break;

	case 'remove_confirm':
		if(is_array(post("inzerce"))) {
			foreach(post("inzerce") as $item) {
				$data = DBInzerce::getSingleInzerat($item);
				DBInzerce::removeInzerat($item);
			}
			notice("Inzeráty odebrány");
			include("files/Admin/Inzerce/Display.inc");
			return;
		}
		break;
}
include("files/Admin/Inzerce/Display.inc");
return;
?>