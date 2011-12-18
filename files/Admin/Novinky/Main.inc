<?php
User::checkPermissionsError(L_ADMIN);

if(!is_numeric(getGetField('id')))
	View::redirect("/member/home", "Novinka s daným ID neexistuje");
DBNovinky::removeNovinka(getGetField('id'));
View::redirect("/member/home", "Novinka odstraněna");
?>