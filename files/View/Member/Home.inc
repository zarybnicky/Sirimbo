<?php header_main('Novinky');?>
<?php notice($this->redirect()->getRedirectMessage());?>
<?php echo $this->partnerRequest()->getAll(); ?>

<?php if($this->data == array()) {
	notice('Žádné novinky');
	return;
}?>

<?php foreach($this->data as $item): ?>
	<div class="no_item">
	<div class="no_text"><?php echo $item['text'];?></div>
	<div class="no_date" style="text-align:right;">
	<?php if($this->canEdit): ?>
		<a href="/admin/novinky/remove?id=<?php echo $item['id'];?>">Odstranit</a> - 
	<?php endif;?>
	<?php echo $item['timestamp'];?></div>
	</div><hr/>
<?php endforeach;?>