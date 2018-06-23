<?= $this->header('Správa uživatelů') ?>
<div class="container full">
<h2>Statistiky</h2>
<br/>
<div class="center">
<?php
echo $this->table()
    ->style('margin:0 auto;')
    ->column('group', '', 'r')
    ->column('count', 'Počet', 'center big')
    ->data($this->data);
?>
<br/>
<a class="btn btn-outline-primary" href="/admin/users">Zpět</a>
</div>
</div>
