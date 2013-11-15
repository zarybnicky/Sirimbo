<?php echo $this->render('src/application/View/Member/Clenove/Navigation.inc');?>
<h2>Seznam skupin</h2>
<?php
echo $this->table()
    ->data($this->data)
    ->column('header', 'Název')
    ->column('userCount', 'Počet členů')
    ->column('description', 'Popis');
?>