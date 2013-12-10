<?php echo $this->render('src/application/View/Member/Clenove/Navigation.inc');?>
<h2>Členové abecedně</h2>
<?php
echo (new TKOlomouc\View\Helper\Table())
    ->column('index', '', 'r')
    ->column('fullName', 'Jméno')
    ->data($this->data);
