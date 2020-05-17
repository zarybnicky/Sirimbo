<div class="container">
  <?php
  $data = $this->data;
  unset($data['info']);
  unset($data['dokumenty']);
  echo $this->render('files/View/Member/Akce/Header.inc', $data);
  ?>

  <div style="text-align:center;">
    <?php if (empty($this->data['items'])) : ?>
      Nikdo není přihlášený<br/>
    <?php else: ?>
      <table style="margin:0 auto;text-align:left">
        <tr>
          <th>Jméno</th>
          <th>Příjmení</th>
          <th>Skupiny</th>
        </tr>
        <?php foreach ($this->data['items'] as $item) : ?>
          <tr>
            <td><?= $item['u_jmeno'] ?></td>
            <td><?= $item['u_prijmeni'] ?></td>
            <td><?= $item['s_name'] ?></td>
          </tr>
        <?php endforeach ?>
      </table>
    <?php endif ?>
    <a class="btn btn-outline-primary" href="/member/akce">Zpět</a>
  </div>
</div>
