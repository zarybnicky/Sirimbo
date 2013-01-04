<?php
class Controller_Member_Clenove implements Controller_Interface {
    function view($id = null) {
        header_main("Členové");

        if($id && ($data = DBUser::getUserData($id))) {
        	echo '<b>', $data['u_jmeno'], ' ', $data['u_prijmeni'], '</b><br/>';
        	echo 'E-mail: ', $data['u_email'], '<br/>';
        	echo 'Telefon: ', $data['u_telefon'], '<br/>';
        	echo 'Poznámky: ', $data['u_poznamky'], '<br/>';
        	echo 'Variabilní symbol: ', User::var_symbol($id), '<br/>';
        	echo '<a href="', Request::getReferer(), '">Zpět</a>';
        	return;
        }
        
        if(!TISK) {
        	echo '<form action="', Request::getURI(), '" method="get">';
        	echo 'Zobrazení:&nbsp;';
        	echo Helper::get()->select()
        		->get()->name('f')
        		->option('skupiny', 'podle skupin')
        		->option('dancer', 'tanečníci')
        		->option('all', 'všichni');
        	echo '<button type="submit">Odeslat</button>';
        	echo '</form>';
        }
        if(!get('f')) get('f', 'skupiny');
        if(get('f') == 'skupiny') {
        	$skupiny = DBSkupiny::getSkupiny();
        	
        	foreach($skupiny as $key => &$skupina) {
        		if(!$skupina['us_platba_mesic'] &&
        			!$skupina['us_platba_ctvrtrok'] && !$skupina['us_platba_pulrok']) {
        			unset($skupiny[$key]);
        			continue;
        		}
        		$skupina['users'] = DBUser::getUsersBySkupina($skupina['us_id']);
        	}
        	unset($skupina);
        	function sortByLength($arr1, $arr2) {
        		$c1 = count($arr1['users']);$c2 = count($arr2['users']);
        		return $c1 > $c2 ? -1 : $c1 == $c2 ? 0 : 1;
        	}
        	usort($skupiny, 'sortByLength');
        	
        	$content = array(array(), array());
        	$count = array(0, 0);
        	foreach($skupiny as $skupina) {
        		$min = 0;
        		foreach($count as $k => $i)
        			if($i < $count[$min]) $min = $k;
        		$content[$min][] = $skupina;
        		$count[$min] += count($skupina['users']);
        	}
        	echo '<table style="width:100%;border:none;background:inherit;">';
        	echo '<tr style="border:none;background:inherit;">';
        	foreach($content as $row) {
        		echo '<td style="border:none;background:inherit;vertical-align:top;">';
        		foreach($row as $skupina) {
        			header_minor(getColorBox($skupina['us_color'], $skupina['us_popis']) .
        				'&nbsp;&nbsp;' . $skupina['us_popis'] . ' (' . count($skupina['users']) . ')');
        			
        			if(!empty($skupina['users'])) {
        				echo '<table style="width:100%">';
        				echo '<tr class="little">';
        				echo '<td>Jméno</td><td style="width:55px;">Zaplaceno</td><td>Platí do</td>';
        				echo '</tr>';
        				foreach($skupina['users'] as $user) {
        					echo '<tr>';
        					echo '<td><a href="clenove/', $user['u_id'], '">',
        						'<img src="/style/person-small.png" alt="" style="margin-bottom:-2px"/></a>&nbsp;',
        						$user['u_prijmeni'], ', ', $user['u_jmeno'], '</td>';
        		
        					echo '<td style="width:25px;">';
        					if($user['up_id'] && strcmp($user['up_plati_do'], date('Y-m-d')) >= 0)
        						echo '<span style="color:#0B0;">ANO</span>';
        					else
        						echo '<span style="color:#B00;font-weight:bold;">NE</span>';
        					echo '</td>';
        					
        					echo '<td style="width:75px;">', (($user['up_id'] &&
        						strcmp($user['up_plati_do'], date('Y-m-d')) >= 0) ?
        						formatDate($user['up_plati_do']) : '---'), '</td>';
        					echo '</tr>';
        				}
        				echo '</table>';
        			}
        			echo '<br/>';
        		}
        		echo '</td>';
        	}
        	echo '</table>';
        	return;
        }
        
        switch(get('f')) {
        	case 'all': $users = DBUser::getActiveUsers(L_ALL); break;
        	case 'dancer':
        	default: $users = DBUser::getActiveDancers(); break;
        }
        echo '<br/>';
        echo 'Celkem:&nbsp;<span class="big">', count($users), '</span><br/>';
        echo '<table>';
        $i = 0;
        foreach($users as $user) {
        	echo '<tr>';
        	echo '<td>&nbsp;', ++$i, '.</td>';
        	echo '<td><a href="/member/clenove/', $user['u_id'], '">';
        	echo '<a href="clenove/', $user['u_id'], '">',
        		'<img src="/style/person-small.png" alt="" style="margin-bottom:-2px"/></a>&nbsp;',
        		$user['u_prijmeni'] . ', ' . $user['u_jmeno'];
        	echo '</td>';
        	if(get('f') == 'dancer') {
        		echo '<td>';
        		echo getColorBox($user['us_color'], $user['us_popis']), '&nbsp;', $user['us_popis'];
        		echo '</td>';
        	}
        	echo '</tr>';
        }
        echo '</table>';
        echo '<br/>';
    }
}
?>