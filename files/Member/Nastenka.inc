<div class="h_section">Upozornění</div>
<?php
User::checkPermissionsError(L_USER);
//TODO: Paging, get(OD,DO)
$zpravy = Database::getNastenka();
foreach($zpravy as $item) {
	echo "<hr/><div class=\"up_item\">";
	echo "<div class=\"up_nadpis\" style=\"float:left;width:50%;\"><b>", $item["up_nadpis"], "</b></div>";
	echo "<div class=\"up_detaily\" style=\"float:right;width:50%;\">";
	echo "<div class=\"up_kdo\">", $item["u_jmeno"], " ", $item["u_prijmeni"], "</div>";
	echo "<div class=\"up_kdy\">", $item["up_kdy"], "</div>";
	echo "</div>";
	echo "<div class=\"up_text\" style=\"clear:right;\">", $item["up_text"], "</div>";
	echo "</div><hr/>";
}
?>