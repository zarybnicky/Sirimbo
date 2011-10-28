<?php
class DisplayTaS {
	public static function viewTaSHeader($data) {
		echo '<div class="t_info" style="border:1px solid black;">';
		echo '<div class="t_jmeno">Název: ', $data['t_jmeno'], '</div>';
		echo '<div class="t_datum">Datum: ', formatDate($data['t_od']);
		if($data['t_od'] != $data['t_do'])
			echo ' - ', formatDate($data['t_do']);
		echo '</div>';
		echo '<div class="t_moreinfo">Další informace: ', $data['t_info'], '</div>';
		echo '<div class="t_dokumenty">Dokumenty:<br/>';
		$doku = unserialize($data['t_dokumenty']);
		if($doku) {
			foreach($doku as $id) {
				$doku_data = DBDokumenty::getSingleDokument($id);
				echo '<a href="/member/download?id=', $id, '">', $doku_data['d_name'], '</a>';
			}
		} else {
			echo 'Žádné dokumenty';
		}
		echo '</div>';
		echo '</div>';
	}
	
	public static function viewFullTaS($id) {
		$tas = DBTaS::getSingleTaS($id);
		if(empty($tas)) {
			notice('Neexistuje žádný takový tábor/soustředění');
			return;
		}
		$tas_items = DBTas::getTaSItems($id);
		
		echo '<div class="t_wrapper">';
		
		DisplayTaS::viewTaSHeader($tas);
		
		if(empty($tas_items)) {
			echo 'Nikdo není přihlášený';
		} else {
			echo '<table>';
			echo '<tr><td>Jméno</td><td>Příjmení</td><td>Rok narození</td></tr>';
			foreach($tas_items as $par) {
				echo '<tr><td>', $par['ti_jmeno'], '</td><td>', $par['ti_prijmeni'], '</td><td>',
				$par['ti_rok_narozeni'], '</td></tr>';
			}
			echo '</table>';
		}
		echo '</div>';
		echo '<a href="/member/tas">Zpět</a>';
	}
}