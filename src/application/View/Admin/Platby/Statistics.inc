<?php echo $this->render('src/application/View/Admin/Platby/Navigation.inc');?>
<h2>Dočasná statistika - členové podle skupin</h2>
<?php
foreach ($this->data as $skupina) {
    echo (new TKOlomouc\View\Helper\Table())
        ->style('width:100%')
        ->data($skupina['users'])
        ->column('index', '', 'r', 'width:40px')
        ->column('fullName', $skupina['info']['header'])
        ->column('hasPaid', 'Zaplaceno?', '', 'width:90px')
        ->column('amount', 'Částka', '', 'width:200px');
}
