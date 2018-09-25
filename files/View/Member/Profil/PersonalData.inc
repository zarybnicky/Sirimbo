<?= $this->header('Osobní údaje') ?>

<div class="container">
  <form action="" method="POST">
    <table>
      <tr>
        <td>Uživatel: </td>
        <td><?= $this->login ?></td>
      </tr><tr>
        <td>Jméno: </td>
        <td><?= $this->text('jmeno', $this->jmeno) ?></td>
      </tr><tr>
        <td>Příjmení: </td>
        <td><?= $this->text('prijmeni', $this->prijmeni) ?></td>
      </tr><tr>
        <td>Pohlaví: </td>
        <td>
          <?= $this->radio('pohlavi', 'm')->set($this->pohlavi == 'm')->label('Muž') ?>&nbsp;
          <?= $this->radio('pohlavi', 'f')->set($this->pohlavi == 'f')->label('Žena') ?>
        </td>
      </tr><tr>
        <td>E-mail (*): </td>
        <td><?= $this->text('email', $this->email) ?></td>
      </tr><tr>
        <td>Telefon (*): </td>
        <td><?= $this->text('telefon', $this->telefon) ?></td>
      </tr><tr>
        <td>Datum narození: (*)</td>
        <td><?= $this->date('narozeni', $this->narozeni) ?></td>
      </tr><tr>
        <td>Poznámky: </td>
        <td><textarea rows="15" cols="50" name="poznamky"><?= $this->poznamky ?></textarea></td>
      </tr><tr>
        <td>Oprávnění: </td>
        <td><?= User::getGroupName() ?></td>
      </tr><tr>
        <td colspan="2"><?= $this->submit('Upravit')->data('action', 'confirm') ?></td>
      </tr>
    </table>
  </form>

  <a class="btn btn-outline-primary" href="/member/profil">Zpět</a>
</div>
