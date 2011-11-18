<?php
User::checkPermissionsError(L_HOST);
header_main('Detail páru');

$request = explode('/', $_SERVER['REQUEST_URI']);
$id = array_pop($request);
if(is_numeric($id)) {
	DisplayPary::viewFullPar($id);
	return;
} else {
	View::redirect('/member/zebricek');
}
?>