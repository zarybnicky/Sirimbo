<?php
class DisplayRozpis {
	public static function viewRozpisHeader($data) {
		echo '<div class="r_info" style="float:left;border:1px solid black;">';
		echo '<div class="r_trener">';
		echoFullJmeno($data);
		echo '</div>';
		
		if(Permissions::canEditRozpis($data['r_id']))
			echo '<div class="r_edit"><a href="/admin/akce/edit/', $data['r_id'], '">Editovat</a></div>';
		
		echo '<div class="r_datum">', formatDate($data['r_datum']), '</div>';
		echo '<div class="r_kde">', $data['r_kde'], '</div>';
		echo '</div>';
	}
}
?>