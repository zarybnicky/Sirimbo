<div class="container">
  <?php
  unset($data['info']);
  unset($data['dokumenty']);
  new \RenderHelper('files/View/Member/Akce/Header.inc', $data, true);
  ?>

  <div style="text-align:center;">
    <?php if (empty($data['items'])) : ?>
      Nikdo není přihlášený<br/>
    <?php else: ?>
      <table style="margin:0 auto;text-align:left">
        <tr>
          <th>Jméno</th>
          <th>Příjmení</th>
          <th>Skupiny</th>
        </tr>
        <?php foreach ($data['items'] as $item) : ?>
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
