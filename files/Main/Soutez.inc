<?php
if(!empty($_POST)) {
	$f = new Form();
	$f->checkNumeric(post('idt'), 'Zadané IDT není platné');
	$f->checkNotEmpty(post('jmeno'), 'Zadejte Vaše jméno');
	$f->checkNotEmpty(post('prijmeni'), 'Zadejte Vaše příjmení');
	$f->checkNotEmpty(post('1porotce'), 'Zadejte alespoň 1. navrhovaného porotce');
	if($f->isValid()) {
		if(DBSoutez::getByIDT(post('idt')) === false) {
			DBSoutez::addRow(post('idt'), post('jmeno'), post('prijmeni'), post('1porotce'),
				post('2porotce'), post('3porotce'), post('4porotce'), post('5porotce'));
			View::redirect('/home', 'Děkujeme za Vaše návrhy, těšíme se na Vás na naší soutěži.');
		} else {
			notice('Toto IDT už je v databázi, asi už jste odeslali Vaše návrhy');
		}
	}
}
header_main("VOLBA POROTY NA LIGU - Přáslavický pohár 2013");
?>
Vážení tanečníci,<br/><br/>

rádi bychom Vám dali možnost podílet se na složení odborné poroty Taneční ligy,
která se společně s postupovými soutěžemi dospělých párů tříd D,C,B a A bude
konat 25.5. v prostorách nového <b>sportovního centra Přáslavic u Olomouce</b>.<br/><br/>

Každý tanečník <b>mezinárodní třídy</b>, registrovaný v Českém svazu tanečního sportu,
má možnost dát hlas <b>pěti porotcům první třídy</b>.<br/><br/>

<a href="http://www.csts.cz/cs/Evidence/SeznamPorotcu" target="_blank">SEZNAM POROTCŮ NAJDETE ZDE</a><br/><br/>

Hlasování bude probíhat <b>od 9.2. do 20.2. 2013</b>. Po tomto datu budou hlasy sečteny a pět porotců
s nejvíce hlasy bude osloveno zástupcem pořadatele. Chceme tím dát najevo, že
soutěž pořádáme pro Vás, tanečníky, a na Vašem názoru nám záleží.<br/><br/>

Za pořádající Taneční klub Olymp Olomouc<br/>
Miroslav Hýža<br/><br/>
<style type="text/css">
table.k {margin:0 auto;}
table.k td.r {padding-right:10px;text-align:right;}
</style>
<?php
if(is_object($f) && !$f->isValid()) {
	notice(implode('<br/>', $f->getMessages()));
}
?>
<form action="<?php echo Request::getURI();?>#form" method="POST">
<table class="k" id="form">
	<tr>
		<td class="r">IDT (*): </td>
		<td><input type="text" name="idt" value="<?php echo post("idt"); ?>" /></td>
	</tr><tr>
		<td class="r">Jméno (*): </td>
		<td><input type="text" name="jmeno" value="<?php echo post("jmeno"); ?>" /></td>
	</tr><tr>
		<td class="r">Příjmení (*): </td>
		<td><input type="text" name="prijmeni" value="<?php echo post("prijmeni"); ?>" /></td>
	</tr><tr>
		<td class="r">1. porotce (*): </td>
		<td><input type="text" name="1porotce" value="<?php echo post("1porotce"); ?>" /></td>
	</tr><tr>
		<td class="r">2. porotce: </td>
		<td><input type="text" name="2porotce" value="<?php echo post("2porotce"); ?>" /></td>
	</tr><tr>
		<td class="r">3. porotce: </td>
		<td><input type="text" name="3porotce" value="<?php echo post("3porotce"); ?>" /></td>
	</tr><tr>
		<td class="r">4. porotce: </td>
		<td><input type="text" name="4porotce" value="<?php echo post("4porotce"); ?>" /></td>
	</tr><tr>
		<td class="r">5. porotce: </td>
		<td><input type="text" name="5porotce" value="<?php echo post("5porotce"); ?>" /></td>
	</tr><tr>
		<td class="r"><button type="submit">Uložit</button></td>
		<td>Pole označená (*) jsou povinná</td>
	</tr>
</table>
</form>