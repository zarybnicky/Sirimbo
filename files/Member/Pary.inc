<?php
User::checkPermissionsError(L_HOST);
header_main('Detail páru');

if(Request::getID()) {
	DisplayPary::viewFullPar(Request::getID());
	return;
} else {
	View::redirect('/member/zebricek');
}
?>