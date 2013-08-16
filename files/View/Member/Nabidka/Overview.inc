<?php header_main('Nabídka tréninků');?>
<?php notice($this->redirect()->getRedirectMessage());?>

<style type="text/css">
.unit {background:inherit;border:none;vertical-align:top;border-bottom:1px dotted #999;padding:10px 5px;}
</style>

<table style="width:100%;">
<?php foreach(array_chunk($this->data, 2) as $chunk):?>
	<tr>
	<?php foreach($chunk as $data):?>
		<td class="unit">
		<form action="" method="post">
		<?php echo $this->render('files/View/Member/Nabidka/Header.inc', $data);?>
		<table style="margin:0 auto;">
			<tr>
				<td>Tanečník</td>
				<td>Počet hodin</td>
			</tr>
			<?php foreach($data['items'] as $row): ?>
			<tr>
				<td><?php echo $row['fullName'];?></td>
				<td><?php echo $row['hourCount'];?></td>
				<?php if($row['canDelete']): ?>
				<td><button type="submit" name="un_id" value="<?php echo $row['deleteTicket'];?>">&times;</button></td>
				<?php endif;?>
			</tr>
			<?php endforeach;?>
		</table>
		<div style="text-align:center;">
		<?php if($data['canAdd']): ?>
			Počet hodin: <input type="text" name="hodiny" size="2" value="<?php post('hodiny');?>" />
			<input type="hidden" name="id" value="<?php echo $data['id'];?>" />
			<button type="submit">Přidat</button>
		<?php endif;?>
		</div>
		</form>
		</td>
	<?php endforeach;?>
<?php endforeach;?>
</table>