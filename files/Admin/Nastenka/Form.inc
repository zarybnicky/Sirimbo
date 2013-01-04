<?php
if(Request::getAction() == 'add')
	header_minor("Přidat příspěvek");
else
	header_minor("Upravit příspěvek");

if(isset($f))
	notice(implode('<br/>', $f->getMessages()));
?>
<form action="<?php echo Request::getURI();?>" method="POST">
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
		$skupiny = DBSkupiny::getSkupiny();
		foreach($skupiny as $skupina) {
            if($skupina['us_platba_mesic'] > 0 || $skupina['us_platba_ctvrtrok'] > 0 || $skupina['us_platba_pulrok'] > 0)
        		echo getCheckbox('sk-' . $skupina['us_id'], '1'), ' ',
        			getColorBox($skupina['us_color'], $skupina['us_popis']), ' ',
        			$skupina['us_popis'], '<br/>';
		}
		echo getReturnInput();
		?></td>
	</tr><tr>
		<td><button type="submit"><?php
			if(Request::getAction() == 'add') echo 'Přidat';
			else echo 'Upravit';
		?></button></td>
		<td>Pole označená (*) jsou povinná</td>
	</tr>
</table>
</form>

<a href="<?php echo Request::getReferer();?>">Zpět</a>