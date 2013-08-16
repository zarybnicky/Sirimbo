<?php
include_once('files/Controller/Member.php');
class Controller_Member_Clenove extends Controller_Member {
	function __construct() {
		Permissions::checkError('users', P_VIEW);
	}
	function view($id = null) {
		if($id && ($data = DBUser::getUserData($id))) {
			$this->render('files/View/Member/Clenove/Single.inc', array(
					'fullName' => $data['u_jmeno'] . ' ' . $data['u_prijmeni'],
					'email' => $data['u_email'],
					'telefon' => $data['u_telefon'],
					'poznamky' => $data['u_poznamky'],
					'varSymbol' => User::var_symbol($id),
					'referer' => Request::getReferer()
			));
			return;
		}
		if(!get('f')) get('f', 'skupiny');
		
		if(get('f') == 'skupiny') {
			$skupiny = DBSkupiny::getSkupiny(true);
			foreach($skupiny as $key => &$skupina) {
				if(!$skupina['us_platba_mesic'] && !$skupina['us_platba_ctvrtrok'] && !$skupina['us_platba_pulrok']) {
					unset($skupiny[$key]);
					continue;
				}
				$users = DBUser::getUsersBySkupina($skupina['us_id'], true);
				$users = $this->__getUserRenderData($users);
				
				$new_data = array(
						'header' => (getColorBox($skupina['us_color'], $skupina['us_popis']) .
								'&nbsp;&nbsp;' . $skupina['us_popis'] . ' (' . count($users) . ')'),
						'users' => $users,
						'count' => $skupina['us_count']
				);
				$skupina = $new_data;
			}unset($skupina);
			
			//Sort into two (or more...) columns
			$content = array(array(), array());
			$count = array(0, 0);
			foreach($skupiny as $skupina) {
				$min = 0;
				foreach($count as $k => $i)
					if($i < $count[$min]) $min = $k;
				$content[$min][] = $skupina;
				$count[$min] += $skupina['count'];
			}
			$this->render('files/View/Member/Clenove/Overview.inc', array(
					'showMenu' => !TISK,
					'viewType' => get('f'),
					'data' => $content
			));
		} else {
			switch(get('f')) {
				case 'all':
					$users = DBUser::getActiveUsers(); break;
				case 'dancer':
				default:
					$users = DBUser::getActiveDancers(); break;
			}
			$users = $this->__getUserRenderData($users);
			$this->render('files/View/Member/Clenove/Overview.inc', array(
					'showMenu' => !TISK,
					'viewType' => get('f'),
					'users' => $users,
					'count' => count($users),
					'showGroup' => get('f') == 'dancer'
			));
		}
	}
	private function __getUserRenderData($data) {
		foreach($data as &$user) {
			$new_data = array(
					'id' => $user['u_id'],
					'fullName' => $user['u_prijmeni'] . ', ' . $user['u_jmeno'],
					'zaplaceno' => ($user['up_id'] && strcmp($user['up_plati_do'], date('Y-m-d')) >= 0),
					'groupDescription' => (getColorBox($user['us_color'], $user['us_popis']) . '&nbsp;' . $user['us_popis'])
			);
			$new_data['platbaDatum'] = ($new_data['zaplaceno'] ? formatDate($user['up_plati_do']) : '---');
			$user = $new_data;
		}
		return $data;
	}
}
?>