<?php echo $this->render('src/application/View/Member/Clenove/Navigation.inc');?>
<h2>Členové podle skupin</h2>
<?php
foreach ($this->data as $skupina) {
    echo '<div style="width:49%;float:', $skupina['info']['align'], '">';
    echo (new TKOlomouc\View\Helper\Table())
        ->style("width:100%")
        ->column('index', '', 'r', 'width:40px')
        ->column('fullName', $skupina['info']['header'])
        ->column('hasPaid', 'Zaplaceno?', '', 'width:40px')
        ->data($skupina['users']);
    echo '</div>';
}
