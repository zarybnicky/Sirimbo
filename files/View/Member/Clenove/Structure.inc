<?php echo $this->render('files/View/Member/Clenove/Navigation.inc');?>
<div class="container full">
<h2>Členové podle skupin</h2>
<?php
foreach ($this->data as $skupina) {
    echo '<div style="width:49%;float:', $skupina['info']['align'], '">';
    echo $this->table()
        ->style("width:100%")
        ->data($skupina['users'])
        ->column('index', '', 'r', 'width:40px')
        ->column('fullName', $skupina['info']['header'])
        ->column('hasPaid', 'Zaplaceno?', '', 'width:40px');
    echo '</div>';
}
?>
</div>
