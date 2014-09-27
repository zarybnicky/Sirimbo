<div class="container">
<h1>Správa uživatelů</h1>
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
<a href="/admin/users">Zpět</a>
</div>
</div>
