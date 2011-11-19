<?php
class DisplayRozpis {
	public static function viewRozpisHeader($data) {
		echo '<div class="r_info" style="float:left;border:1px solid black;">';
		echo '<div class="r_trener">';
		echoFullJmeno($data);
		if(Permissions::canEditRozpis($data['r_id']))
			echo ' - <a href="/admin/rozpis/edit/', $data['r_id'], '">Editovat</a>';
		echo '</div>';
		
		echo '<div class="r_datum">', formatDate($data['r_datum']), '</div>';
		echo '<div class="r_kde">', $data['r_kde'], '</div>';
		echo '</div>';
	}
}
?>