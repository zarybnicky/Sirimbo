<?php
class DisplayRozpis {
	public static function viewRozpisHeader($data) {
		echo '<div class="trenink_header">';
		echo '<div class="nadpis">';
		echoFullJmeno($data);
		echo '</div>';
		echo '<div style="letter-spacing:1px;font-weight:bold;">', formatDate($data['r_datum']), '</div>';
		
		if(Permissions::canEditRozpis($data['r_id'])) {
			echo '<span class="big">Admin:</span> ';
			echo '<a href="/admin/rozpis/edit/', $data['r_id'], '">obecné</a>, ';
			echo '<a href="/admin/rozpis/detail/', $data['r_id'], '">tréninky</a>';
		}
		
		echo '<div>', $data['r_kde'], '</div>';
		echo '</div>';
	}
}
?>