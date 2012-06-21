<?php
User::checkPermissionsError(L_ADMIN);
ob_clean();

$permissions = post('permissions');
$jmeno = post('jmeno');
$prijmeni = post('prijmeni');

if(!($user = DBUser::getUserByFullName($jmeno,$prijmeni))) {
	$login = preg_replace('/[^\x20-\x7F]*/','', strtolower($prijmeni)) . '_' .
		preg_replace('/[^\x20-\x7F]*/','', strtolower($jmeno));
	list($user_id, $par_id) = DBUser::addTemporaryUser($login, $jmeno, $prijmeni, $permissions);
	
	echo json_encode(array('user_id' => $user_id, 'par_id' => $par_id, 'jmeno' => $jmeno,
		'prijmeni' => $prijmeni, 'temporary' => 1));
} else {
	if(!($partner = DBPary::getLatestPartner($user['u_id'], $user['u_pohlavi']))) {
		$par_id = DBPary::noPartner($user['u_id']);
	} else {
		$par_id = $partner['p_id'];
	}
	echo json_encode(array('user_id' => $user['u_id'], 'par_id' => $par_id, 'jmeno' => $user['u_jmeno'],
		'prijmeni' => $user['u_prijmeni'], 'temporary' => 0));
}
exit;
?>