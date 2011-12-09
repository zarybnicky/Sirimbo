<?php
User::checkPermissionsError(L_ADMIN);
header_main("Správa inzerce");

notice(View::getRedirectMessage());

$request = explode('/', $_SERVER['REQUEST_URI']);
$action = array_slice($request, 3);
if(empty($action)) $action[] = '';

switch($action[0]) {
	case 'add':
		if(empty($_POST)) {
			include('files/Admin/Inzerce/Form.inc');
			return;
		} else {
			$od = getPostField('od_year') . '-' . getPostField('od_month') . '-' . getPostField('od_day');
			$do = getPostField('do_year') . '-' . getPostField('do_month') . '-' . getPostField('do_day');
			
			if($do == '0000-00-00' || strcmp($od, $do) > 0)
				$do = $od;
			
			if(getPostField('reg') > 0 && is_numeric(getPostField('reg')) &&
					($userdata = DBUser::getUserData(getPostField('reg')))) {
				$userid = getPostField('reg');
				$jmeno = $userdata['u_jmeno'];
				$prijmeni = $userdata['u_prijmeni'];
			} else {
				$userid = 0;
				$jmeno = getPostField('jmeno');
				$prijmeni = getPostField('prijmeni');
			}
			$confirmed = '1';
			$visible = (bool) getPostField('visible');
			
			$error = false;
			
			ob_start();
			include('files/Admin/Inzerce/Form.inc');	
			$html = ob_get_clean();
			
			if(!$error) {
				DBInzerce::addInzerat(getPostField('kat'), $userid, $jmeno, $prijmeni,
					getPostField('nadpis'), getPostField('text'), serialize(array()), $od, $do,
					$visible, $confirmed);
				View::redirect('/admin/inzerce', 'Inzerát přidán');
			} else {
				echo $html;
				return;
			}
		}
		break;
	
	case 'edit':
		$id = isset($action[1]) ? $action[1] : '';
		
		if(!$id || !is_numeric($id) || !($data = DBInzerce::getSingleInzerat($id)))
			View::redirect('/admin/inzerce', 'Inzerát s takovým ID neexistuje');
		
		if(empty($_POST)) {
			$_POST['kat'] = $data['i_kat'];
			$_POST['reg'] = $data['i_reg'];
			$_POST['jmeno'] = $data['i_jmeno'];
			$_POST['prijmeni'] = $data['i_prijmeni'];
			$_POST['nadpis'] = $data['i_nadpis'];
			$_POST['text'] = $data['i_text'];
			list($od_year, $od_month, $od_day) = explode('-', $data['i_od']);
			$_POST['od_year'] = $od_year;
			$_POST['od_month'] = $od_month;
			$_POST['od_day'] = $od_day;
			list($do_year, $do_month, $do_day) = explode('-', $data['i_do']);
			$_POST['do_year'] = $do_year;
			$_POST['do_month'] = $do_month;
			$_POST['do_day'] = $do_day;
			$_POST['visible'] = $data['i_visible'];
			$_POST['confirmed'] = $data['i_confirmed'];
			
			include('files/Admin/Inzerce/Form.inc');
			return;
		} else {
			$od = getPostField('od_year') . '-' . getPostField('od_month') . '-' . getPostField('od_day');
			$do = getPostField('do_year') . '-' . getPostField('do_month') . '-' . getPostField('do_day');
			
			if($do == '0000-00-00' || strcmp($od, $do) > 0)
				$do = $od;
			
			if(getPostField('reg') > 0 && is_numeric(getPostField('reg'))) {
				$userdata = DBUser::getUserData(getPostField('reg'));
				
				$userid = getPostField('reg');
				$jmeno = $userdata['u_jmeno'];
				$prijmeni = $userdata['u_prijmeni'];
			} else {
				$userid = 0;
				$jmeno = getPostField('jmeno');
				$prijmeni = getPostField('prijmeni');
			}
			$confirmed = '1';
			$visible = (bool) getPostField('visible');
			
			$error = false;
			
			ob_start();
			include('files/Admin/Inzerce/Form.inc');	
			$html = ob_get_clean();
			
			if(!$error) {
				DBInzerce::editInzerat($id, getPostField('kat'), $userid, $jmeno, $prijmeni,
					getPostField('nadpis'), getPostField('text'), serialize(array()), $od, $do, $visible,
					$confirmed);
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
			if(getPostField('action') == 'confirm') {
				if(is_array(getPostField('inzerce'))) {
					foreach(getPostField('inzerce') as $id) {
						$visible = getPostField($id . '-visible');
						DBInzerce::confirmInzerat($id, (bool) $visible);
					}
				}
				View::redirect('/admin/inzerce', 'Inzeráty potvrzeny');
				return;
			} elseif(getPostField('action') == 'remove') {
				echo '<form action="/admin/inzerce" method="POST">';
				echo 'Opravdu chcete odstranit inzeráty:<br/><br/>';
				foreach(getPostField('inzerce') as $id) {
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

switch(getPostField("action")) {
	case 'edit':
		$inzerce = getPostField('inzerce');
		if($inzerce[0]) {
			header('Location: /admin/inzerce/edit/' . $inzerce[0]);
			return;
		}
		break;

	case 'remove':
		if(is_array(getPostField('inzerce'))) {
			echo '<form action="/admin/inzerce" method="post">';
			echo 'Opravdu chcete odstranit inzeráty:<br/><br/>';
			foreach(getPostField('inzerce') as $id) {
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
		if(is_array(getPostField("inzerce"))) {
			foreach(getPostField("inzerce") as $item) {
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