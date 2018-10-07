<div class="container">
  <form action="" method="POST">
    <a href="/admin/nastenka/add" class="btn btn-primary">Přidat</a>
<?php
if (empty($this->data)) {
    echo $this->notice('Žádné příspěvky na nástěnkce');
    return;
}
$tbl = $this->table()->style('width:100%')->data($this->data);
if ($this->showButtonsCol) {
    $tbl->column('buttons', '');
}
echo $tbl->column('header', 'Nadpis')
        ->column('groups', 'Skupiny')
        ->column('fullName', 'Přidal')
        ->column('timestampAdd', 'Přidáno', '', 'min-width:83px;');
?>
  </form>
  <div style="text-align:center;"><?= $this->navigation ?></div>
</div>
