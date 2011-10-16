<?php
//TODO: Novinky,DB
User::checkPermissionsError(L_HOST);

DisplayPary::viewPartnerRequests(DBPary::getPartnerRequestsForMe(User::getUserID()),
	DBPary::getPartnerRequestsByMe(User::getUserID()));

echo "Vítejte na domovské stránce uživatele: " . User::getUserName();
?>