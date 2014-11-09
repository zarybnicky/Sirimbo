<?php echo $this->render('files/View/Member/Clenove/Navigation.inc');?>
<div class="container full">
<h2>Seznam skupin</h2>
<?php
echo $this->table()
    ->data($this->data)
    ->column('header', 'Název')
    ->column('userCount', 'Počet členů')
    ->column('description', 'Popis');
?>
</div>
