<?php
//TODO: Novinky,DB
User::checkPermissionsError(L_USER);
echo "Vítejte na domovské stránce uživatele: " . User::getUserName();
?>