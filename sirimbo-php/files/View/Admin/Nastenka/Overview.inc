<div class="container">
  <form action="" method="POST">
    <a href="/admin/nastenka/add" class="btn btn-primary">Přidat</a>
<?php
if (empty($data)) {
    echo \Utils::notice('Žádné příspěvky na nástěnkce');
    return;
}
$tbl = (new TableHelper($data))->style('width:100%');
if ($showButtonsCol) {
    $tbl->column('buttons', '');
}
echo $tbl->column('header', 'Nadpis')
        ->column('groups', 'Skupiny')
        ->column('fullName', 'Přidal')
        ->column('timestampAdd', 'Přidáno', '', 'min-width:83px;');
?>
  </form>
  <div style="text-align:center;"><?= $navigation ?></div>
</div>
