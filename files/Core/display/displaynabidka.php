<?php
class DisplayNabidka {
	public static function viewNabidkaHeader($data, $obsazeno) {
		echo '<div class="n_info" style="border:1px solid black;">';
		echo '<div class="n_trener">';
		echoFullJmeno($data);
		echo '</div>';
		echo '<div class="n_datum">', formatDate($data['n_od']);
		if($data['n_od'] != $data['n_do'])
			echo ' - ', formatDate($data['n_do']);
		echo '</div>';
		echo '<div class="n_hodiny_all">Celkem hodin: ', $data['n_pocet_hod'], '</div>';
		echo '<div class="n_hodiny_obs">Obsazených hodin: ', (int) $obsazeno, '</div>';
		echo '<div class="n_hodiny_vol">Volných hodin: ', $data['n_pocet_hod'] - (int) $obsazeno, '</div>';
		echo '</div>';
	}
}