<div class="container">
    <form action="" method="GET">
      <?= $this->select('kat')
               ->set($this->kat)
               ->option('', '---VŠE---')
               ->option('1', 'Schůze, rady')
               ->option('2', 'Soutěže')
               ->option('3', 'Tábory')
               ->option('0', 'Ostatní');
      ?>
      <?= new \SubmitHelper('Zobrazit') ?>
    </form>

  <table>
    <tr>
      <th>Jméno souboru</th>
      <th>Soubor</th>
      <th>Kategorie</th>
      <th>Uploadoval</th>
    </tr>
    <?php foreach ($this->data as $item): ?>
      <tr>
        <td><?= $item['name'] ?></td>
        <td><a href="/member/download?id=<?= $item['id'] ?>">
          <?= $item['fileName'] ?>
        </a></td>
        <td><?= $item['kategorie'] ?></td>
        <td><?= $item['uploadedBy'] ?></td>
      </tr>
    <?php endforeach ?>
  </table>
</div>
