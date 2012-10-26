<?php
User::checkPermissionsError(L_ADMIN);
ob_clean();

$permissions = post('permissions');
$jmeno = post('jmeno');
$prijmeni = post('prijmeni');
$narozeni = post('narozeni');
$rok = explode('-', $narozeni);
$rok = array_shift($rok);

$login = preg_replace('/[^a-zA-Z0-9.-_]*/','', strtolower($prijmeni)) . '_' .
	preg_replace('/[^a-zA-Z0-9.-_]*/','', strtolower($jmeno));

if(!($id = DBUser::getUserID($login))) {
	list($user_id, $par_id) = DBUser::addTemporaryUser($login, $jmeno, $prijmeni, $narozeni, $permissions);
	
	echo json_encode(array('user_id' => $user_id, 'par_id' => $par_id, 'jmeno' => $jmeno,
		'prijmeni' => $prijmeni, 'narozeni' => $narozeni, 'rok' => $rok, 'temporary' => 1));
} else {
	$data = DBUser::getUserData($id);
	if(!($partner = DBPary::getLatestPartner($data['u_id'], $data['u_pohlavi']))) {
		$par_id = DBPary::noPartner($user['u_id']);
	} else {
		$par_id = $partner['p_id'];
	}
	echo json_encode(array('user_id' => $data['u_id'], 'par_id' => $par_id, 'jmeno' => $data['u_jmeno'],
		'prijmeni' => $data['u_prijmeni'], 'narozeni' => $data['u_narozeni'],
		'rok' => $rok, 'temporary' => 0));
}
exit;
?>