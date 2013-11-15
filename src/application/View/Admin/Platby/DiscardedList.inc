<?php
echo $this->render('src/application/View/Admin/Platby/Navigation.inc');
echo '<h2>Vyřazené platby</h2>';

echo '<a style="display:block;margin-bottom:8px;" href="?list=all">Všechny platby</a>';
if (count($this->groupByDate) > 0) {
    echo '<span class="big">Platby podle rozpoznaného data:</span>';
    echo '<ul style="list-style:disc outside;">';
    foreach ($this->groupByDate as $yearValue => $year) {
        $href = '?list=date&year=' . $yearValue;
        echo '<li style="margin-left:1em"><a href="', $href, '">', $year['name'], '</a></li>';
        
        if (!isset($year['months']))
            continue;
        
        echo '<ul style="list-style:disc outside;margin-left:1em;">';
        foreach ($year['months'] as $monthValue => $month) {
            echo '<li style="margin-left:1em"><a href="', $href, '&month=', $monthValue, '">', $month, '</a>';
        }
        echo '</li></ul>';
    }
    echo '</ul><br/>';
}
if (count($this->groupByAmount) > 0) {
    echo '<span class="big">Platby podle rozpoznané částky:</span>';
    echo '<ul style="list-style:disc outside;">';
    foreach ($this->groupByAmount as $value => $name) {
        echo '<li style="margin-left:1em"><a href="?list=amount&amount=', $value, '">', $name, '</a></li>';
    }
    echo '</ul>';
}
?>