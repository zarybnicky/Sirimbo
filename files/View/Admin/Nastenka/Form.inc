<?php
header_main('Správa nástěnky');
if($this->action == 'add')
	header_minor("Přidat příspěvek");
else
	header_minor("Upravit příspěvek");
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
			if($item['us_platba_mesic'] > 0 || $item['us_platba_ctvrtrok'] > 0 || $item['us_platba_pulrok'] > 0)
				echo getCheckbox('sk-' . $item['us_id'], '1'), ' ',
					getColorBox($item['us_color'], $item['us_popis']), ' ',
					$item['us_popis'], '<br/>';
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