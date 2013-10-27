<?php
class DisplayNabidka
{
    public static function viewNabidkaHeader($data, $obsazeno) {
        echo '<div class="trenink_header">';
        echo '<div class="nadpis">';
        echoFullJmeno($data);
        echo '</div>';
        echo '<div style="letter-spacing:1px;font-weight:bold;">', formatDate($data['n_od']);
        if ($data['n_od'] != $data['n_do'])
            echo ' - ', formatDate($data['n_do']);
        echo '</div>';

        if (Permissions::check('nabidka', P_OWNED, $data['n_trener'])) {
            echo '<span style="color:#572E00;font-size:115%;">Admin: </span>';
            echo '<a href="/admin/nabidka/edit/', $data['n_id'], '">obecné</a>, ';
            echo '<a href="/admin/nabidka/detail/', $data['n_id'], '">tréninky</a>';
        }
        if ($data['n_max_pocet_hod'] > 0)
            echo '<div><span class="little">Maximálne hodin/pár:</span>',
                '<span class="nadpis">', $data['n_max_pocet_hod'], '</span></div>';
        echo '<div><span class="little">Celkem hodin: </span>',
            '<span class="nadpis">', $data['n_pocet_hod'], '</span></div>';
        echo '<div><span class="little">Obsazených hodin: </span>',
            '<span class="nadpis">', (int) $obsazeno, '</span></div>';
        echo '<div><span class="little">Volných hodin: </span>',
            '<span class="nadpis">', $data['n_pocet_hod'] - (int) $obsazeno, '</span></div>';
        echo '</div>';
    }
}