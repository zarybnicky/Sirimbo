<?php
header_main('Inzerce');

notice(View::getRedirectMessage());

$request = explode('/', $_SERVER['REQUEST_URI']);
$action = array_slice($request, 2);
if(empty($action)) $action[] = '';

switch($action[0]) {
	case 'posledni':
		header_minor('Poslední inzeráty');
		$result = DisplayInzerce::viewInzerce(true, true, INZERCE_ALL, INZERCE_COUNT);
		if(!$result)  {
			notice('Žádné inzeráty');
		}
		return;
	case 'prodam':
		header_minor('Prodám');
		$result = DisplayInzerce::viewInzerce(true, true, INZERCE_PRODAM);
		if(!$result)  {
			notice('Žádné inzeráty');
		}
		return;
	case 'koupim':
		header_minor('Koupím');
		$result = DisplayInzerce::viewInzerce(true, true, INZERCE_KOUPIM);
		if(!$result)  {
			notice('Žádné inzeráty');
		}
		return;
	case 'partner':
		header_minor('Hledám partnera');
		$result = DisplayInzerce::viewInzerce(true, true, INZERCE_PARTNER);
		if(!$result)  {
			notice('Žádné inzeráty');
		}
		return;
	case 'partnerka':
		header_minor('Hledám partnerku');
		$result = DisplayInzerce::viewInzerce(true, true, INZERCE_PARTNERKA);
		if(!$result)  {
			notice('Žádné inzeráty');
		}
		return;
	case 'add':
		if(empty($_POST)) {
			include('files/Main/Inzerce/Form.inc');
			return;
		} else {
			$od = getPostField('od_year') . '-' . getPostField('od_month') . '-' . getPostField('od_day');
			$do = getPostField('do_year') . '-' . getPostField('do_month') . '-' . getPostField('do_day');
			
			if($do == '0000-00-00' || strcmp($od, $do) > 0)
				$do = $od;
			
			$jmeno = User::isLogged() ? User::getUserJmeno() : getPostField('jmeno');
			$prijmeni = User::isLogged() ? User::getUserPrijmeni() : getPostField('prijmeni');
			
			$confirmed = User::isLogged() ? '1' : '0';
			$visible = User::isLogged() ? (bool) getPostField('visible') : '0';
			
			$error = false;
			
			ob_start();
			include('files/Main/Inzerce/Form.inc');	
			$html = ob_get_clean();
			
			if(!$error) {
				DBInzerce::addInzerat(getPostField('kat'), User::getUserID(), $jmeno, $prijmeni,
					getPostField('nadpis'), getPostField('text'), serialize(array()), $od, $do, $visible,
					$confirmed);
				if(User::isLogged())
					View::redirect("/inzerce/posledni", "Váš inzerát byl přidán");
				else
					View::redirect('/inzerce/posledni', 'Váš inzerát byl uložen a čeká na schválení administrátora');
			} else {
				echo $html;
				return;
			}
		}
	case 'edit':
		$id = isset($action[1]) ? $action[1] : '';
		
		if(!$id || !is_numeric($id) || !($data = DBInzerce::getSingleInzerat($id)))
			View::redirect('/inzerce/posledni', 'Inzerát s takovým ID neexistuje');
		
		if(!Permissions::canEditInzerat($data['i_reg']))
			View::viewError(ER_AUTHORIZATION);
		
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
			
			include('files/Main/Inzerce/Form.inc');
			return;
		} else {
			$od = getPostField('od_year') . '-' . getPostField('od_month') . '-' . getPostField('od_day');
			$do = getPostField('do_year') . '-' . getPostField('do_month') . '-' . getPostField('do_day');
			
			if($do == '0000-00-00' || strcmp($od, $do) > 0)
				$do = $od;
			
			$confirmed = '1';
			$visible = (bool) getPostField('visible');
			
			$error = false;
			
			ob_start();
			include('files/Main/Inzerce/Form.inc');	
			$html = ob_get_clean();
			
			if(!$error) {
				DBInzerce::editInzerat($id, getPostField('kat'), User::getUserID(), User::getUserJmeno(),
					User::getUserPrijmeni(), getPostField('nadpis'), getPostField('text'), serialize(array()),
					$od, $do, $visible, $confirmed);
				View::redirect("/inzerce/posledni", "Inzerát upraven");
			} else {
				echo $html;
				return;
			}
		}
		break;
	case 'remove':
		$id = isset($action[1]) ? $action[1] : '';
		
		if(!$id || !is_numeric($id) || !($data = DBInzerce::getSingleInzerat($id)))
			View::redirect('/inzerce/posledni', 'Inzerát s takovým ID neexistuje');
		
		if(!Permissions::canEditInzerat($data['i_reg']))
			View::viewError(ER_AUTHORIZATION);
		
		if(empty($_POST)) {
			echo '<form action="', $_SERVER['REQUEST_URI'], '" method="post">';
			echo 'Opravdu chcete odstranit inzerát "';
			$data = DBInzerce::getSingleInzerat($id);
			echo $data['i_nadpis'];
			echo '"?<br/>';
			echo '<input type="hidden" name="id" value="', $id, '" />';
			echo '<button type="submit" name="action" value="remove_confirm">Odstranit</button>';
			echo '<a href="/inzerce/posledni">Zpět</a>';
			echo '</form>';
			return;
		} else {
			DBInzerce::removeInzerat($id);
			
			View::redirect("/inzerce/posledni", "Inzerát odebrán");
			return;
		}
		break;
}
?>