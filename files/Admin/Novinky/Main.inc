<?php
User::checkPermissionsError(L_ADMIN);

if(!is_numeric(get('id')))
	View::redirect("/member/home", "Novinka s daným ID neexistuje");
DBNovinky::removeNovinka(get('id'));
View::redirect("/member/home", "Novinka odstraněna");
?>