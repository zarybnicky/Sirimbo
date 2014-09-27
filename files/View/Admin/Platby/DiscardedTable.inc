<div class="container">
<?php
echo $this->render('files/View/Admin/Platby/Navigation.inc');
echo '<h2>Vyřazené platby (' . $this->header . ')</h2>';
echo $this->table()
    ->data($this->data)
    ->columns($this->columns);
?>
</div>
