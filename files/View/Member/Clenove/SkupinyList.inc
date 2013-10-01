<?php echo $this->render('files/View/Member/Clenove/Navigation.inc');?>
<h2>Seznam skupin</h2>
<?php
echo $this->table()
	->data($this->data)
	->column('header', 'Název')
	->column('description', 'Popis');
?>