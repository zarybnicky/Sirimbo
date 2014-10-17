<?php echo $this->render('files/View/Member/Clenove/Navigation.inc');?>
<div class="container">
<h2>Členové abecedně</h2>
<?php
echo $this->table()
    ->data($this->data)
    ->column('index', '', 'r')
    ->column('fullName', 'Jméno');
?>
</div>
