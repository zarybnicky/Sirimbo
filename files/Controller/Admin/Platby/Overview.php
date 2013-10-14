<?php
include_once('files/Controller/Admin/Platby.php');
class Controller_Admin_Platby_Overview extends Controller_Admin_Platby {
	function __construct() {
		Permissions::checkError('platby', P_OWNED);
	}
	function view($id = null) {
		$data = DBUser::getUsersWithSkupinaPlatby();
		$skupiny = array();
		$index = 0;
		$currentID = 0;
		$currentKey = 0;
		foreach($data as $item) {
			if($item['s_id'] != $currentID) {
				$index = 0;
				$currentID = $item['s_id'];
				$currentKey = count($skupiny) - 1;
				$skupiny[$currentKey] = array();
				$skupiny[$currentKey]['info'] = array(
						'header' => '<h3>' . getColorBox($item['s_color_rgb'], $item['s_description']) .
							'&nbsp;&nbsp;' . $item['s_name'] . '</h2>'
				);
				$skupiny[$currentKey]['users'] = array();
			}
			$skupiny[$currentKey]['users'][] = array(
					'index' => ++$index . '.',
					'fullName' => '<a href="/member/clenove/' . $item['u_id'] . '">' .
						'<img src="/style/person-small.png" alt="' . $item['u_jmeno'] . ' ' . $item['u_prijmeni'] .
						'" style="margin-bottom:-2px"/>' .
						'</a>' .
						'&nbsp;' . $item['u_prijmeni'] . ', ' . $item['u_jmeno'],
					'hasPaid' => ($item['pi_id'] ? '<span style="color:green">ANO</span>' : '<span style="font-weight:bold;color:red">NE</span>'),
					'amount' => ($item['pi_amount'] == ($item['pc_amount'] * $item['pg_base']) ?
						'<span style="color:green">' . (int) $item['pi_amount'] . ' Kč</span>' :
						'<span style="font-weight:bold;color:red">' .
							(int) $item['pi_amount'] . ' Kč</span> (' . (int) ($item['pc_amount'] * $item['pg_base']) . ' Kč)')
			);
		}
		foreach($skupiny as &$skupina)
			$skupina['info']['count'] = count($skupina['users']);
		
		$this->render('files/View/Admin/Platby/Statistics.inc', array(
				'data' => $skupiny
		));
	}
}