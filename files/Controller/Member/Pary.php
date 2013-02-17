<?php
class Controller_Member_Pary implements Controller_Interface {
	function __construct() {
		Permissions::checkError('pary', P_VIEW);
	}
    function view($id = null) {
        if($id) {
            header_main('Detail páru');
       	    DisplayPary::viewFullPar($id);
        	return;
        }
        
        header_main('Žebříček párů');
        notice(View::getRedirectMessage());
        
        $pary = DBPary::getActiveParyByHodnoceni();
        if(empty($pary)) {
        	notice("Žádné páry");
        	return;
        }
        
        echo '<table>';
        echo '<tr><td>Pořadí</td><td>Partner</td><td>Partnerka</td><td>Standard</td><td>Latina</td><td>Body</td><td></td></tr>';
        
        $count = count($pary);
        for($i = 1; $i <= $count; $i++) {
        	$par = $pary[$i - 1];
        	echo '<tr>',
        		'<td style="text-align:right;">', $i, '. </td>',
        		'<td>', $par['guy_name'], ' ', $par['guy_surname'], '</td>',
        		'<td>', $par['gal_name'], ' ', $par['gal_surname'], '</td>',
        		'<td>', $par['p_stt_trida'], ' ', $par['p_stt_body'], 'F', $par['p_stt_finale'], '</td>',
        		'<td>', $par['p_lat_trida'], ' ', $par['p_lat_body'], 'F', $par['p_lat_finale'], '</td>',
        		'<td>', $par['p_hodnoceni'], '</td>',
        		'<td><a href="/member/pary/', $par['p_id'], '">Profil</a></td>',
        		'</tr>';
        }
        echo '</table>';
    }
}
?>