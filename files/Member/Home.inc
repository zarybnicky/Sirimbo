<?php
//TODO: Novinky,DB
User::checkPermissionsError(L_HOST);
echo "Vítejte na domovské stránce uživatele: " . User::getUserName();
?>