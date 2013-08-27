<?php header_minor('Moje platby');?>

<?php echo $this->colorBox, '&nbsp;', $this->skupinaData;?><br/>
Variabilní symbol: <?php echo $this->varSymbol;?>
<br/><br/>
Aktuálně: <?php echo $this->zaplaceno ? 'zaplaceno' : 'NEZAPLACENO';?>
<br/><br/>

<?php if(!empty($this->platby)): ?>
<?php header_minor('Historie plateb');?>
<table>
	<tr>
		<td>Skupina</td>
		<td>Období</td>
		<td>Částka</td>
		<td>Placeno</td>
		<td>Platí do</td>
	</tr>
	<?php foreach($this->platby as $platba): ?>
	<tr>
		<td><?php echo $platba['colorBox'];?></td>
		<td><?php echo $platba['obdobi'];?> Kč</td>
		<td><?php echo $platba['castka'];?> Kč</td>
		<td><?php echo $platba['datum'];?></td>
		<td><?php echo $platba['platnost'];?></td>
	</tr>
	<?php endforeach;?>
</table><br/>
<?php endif;?>

<?php header_minor('Přehled skupin');?>
<table>
	<tr>
		<td>Skupina</td>
		<td>Kč/čtvrtrok</td>
		<td>Kč/půlrok</td>
	</tr>
	<?php foreach($this->skupiny as $skupina): ?>
	<tr>
		<td><?php echo $skupina['colorBox'];?>&nbsp;<?php echo $skupina['popis'];?></td>
		<td><?php echo $skupina['castkaCtvrtleti'];?></td>
		<td><?php echo $skupina['castkaPololeti'];?></td>
	</tr>
	<?php endforeach;?>
</table>
<a href="/member/profil">Zpět</a>