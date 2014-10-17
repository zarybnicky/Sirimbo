<?= $this->header('Dokumenty') ?>

<div class="container">
  <?php if ($this->showMenu): ?>
    <form action="" method="GET">
      <?php
      echo $this->select()
                ->get()
                ->name('kat')
                ->option('', '---VŠE---')
                ->option('1', 'Schůze, rady')
                ->option('2', 'Soutěže')
                ->option('3', 'Tábory')
                ->option('0', 'Ostatní');
      ?>
      <button type="submit">Zobrazit</button>
    </form>
  <?php endif; ?>

  <table>
    <tr>
      <th>Jméno souboru</th>
      <th>Soubor</th>
      <th>Kategorie</th>
      <th>Uploadoval</th>
    </tr>
    <?php foreach ($this->data as $item): ?>
      <tr>
        <td><?php echo $item['name'];?></td>
        <td><a href="/member/download?id=<?php echo $item['id'];?>">
          <?php echo $item['fileName'];?>
        </a></td>
        <td><?php echo $item['kategorie'];?></td>
        <td><?php echo $item['uploadedBy'];?></td>
      </tr>
    <?php endforeach;?>
  </table>
</div>
