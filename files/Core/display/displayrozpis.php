<?php
class DisplayRozpis {
	public static function viewRozpisHeader($data) {
		echo '<div class="trenink_header">';
		echo '<div class="nadpis">';
		echoFullJmeno($data);
		echo '</div>';
		if(Permissions::canEditRozpis($data['r_id']))
			echo '<a href="/admin/rozpis/edit/', $data['r_id'], '">Editovat</a>';
		
		echo '<div style="letter-spacing:1px;font-weight:bold;">', formatDate($data['r_datum']), '</div>';
		echo '<div>', $data['r_kde'], '</div>';
		echo '</div>';
	}
}
?>