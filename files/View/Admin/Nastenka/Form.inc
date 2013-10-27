<h1>Správa nástěnky</h1>
<?php
echo '<h2>', ($this->action == 'add' ? 'Přidat příspěvek' : 'Upravit příspěvek'), '</h2>';
notice($this->redirect()->getMessage());
?>
<form action="" method="POST">
<table>
    <tr>
        <td>Nadpis:</td>
        <td><input type="text" name="nadpis" value="<?php echo post("nadpis");?>" />
    </tr><tr>
        <td>Text:</td>
        <td><textarea cols="50" rows="15" name="text"><?php echo post("text");?></textarea></td>
    </tr><tr>
        <td>Uzamknout:</td>
        <td><?php echo getCheckbox('lock');?></td>
    </tr><tr>
        <td>Skupiny:</td>
        <td><?php
        foreach($this->skupiny as $item) {
            if(true) //$item['us_platba_mesic'] > 0 || $item['us_platba_ctvrtrok'] > 0 || $item['us_platba_pulrok'] > 0
                echo getCheckbox('sk-' . $item['s_id'], '1'), ' ',
                    getColorBox($item['s_color_rgb'], $item['s_description']), ' ',
                    $item['s_name'], '<br/>';
        }
        echo getReturnInput();
        ?></td>
    </tr><tr>
        <td><button type="submit"><?php
            echo ($this->action == 'add' ? 'Přidat' : 'Upravit');
        ?></button></td>
        <td>Pole označená (*) jsou povinná</td>
    </tr>
</table>
</form>

<a href="<?php echo $this->returnURL?>">Zpět</a>