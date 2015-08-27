<?= $this->render('files/View/Member/Clenove/Navigation.inc', array('uri' => $this->uri)) ?>
<div class="container full">
<?php
echo $this->table()
    ->data($this->data)
    ->column('index', '', 'r')
    ->column('fullName', 'Jméno');
?>
</div>
