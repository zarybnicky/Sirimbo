<?= $this->header('Osobní údaje') ?>

<div class="container full">
  <?= $this->notice($this->redirect()->getMessage());?>
  <form action="" method="POST">
    <table>
      <tr>
        <td>Uživatel: </td>
        <td><?= $this->login ?></td>
        <td></td>
      </tr><tr>
        <td>Jméno: </td>
        <td><input type="text" name="jmeno" value="<?= $this->jmeno ?>" /></td>
        <td>Max. 40 znaků</td>
      </tr><tr>
        <td>Příjmení: </td>
        <td><input type="text" name="prijmeni" value="<?= $this->prijmeni ?>" /></td>
        <td>Max. 40 znaků</td>
      </tr><tr>
        <td>Pohlaví: </td>
        <td>
          <label><?= $this->radio('pohlavi', 'm')->set($this->pohlavi == 'm') ?>Muž</label>&nbsp;
          <label><?= $this->radio('pohlavi', 'f')->set($this->pohlavi == 'f') ?>Žena</label>
        </td>
        <td></td>
      </tr><tr>
        <td>E-mail (*): </td>
        <td><input type="text" name="email" value="<?= $this->email ?>" /></td>
        <td>Max. 50 znaků, ve formátu e-mail adresy</td>
      </tr><tr>
        <td>Telefon (*): </td>
        <td><input type="text" name="telefon" value="<?= $this->telefon ?>" /></td>
        <td></td>
      </tr><tr>
        <td>Datum narození: (*)</td>
        <td><?= $this->date('narozeni')->set($this->narozeni)->textBox(); ?></td>
        <td></td>
      </tr><tr>
        <td>Poznámky: </td>
        <td><textarea rows="15" cols="50" name="poznamky"><?= $this->poznamky ?></textarea></td>
        <td></td>
      </tr><tr>
        <td>Oprávnění: </td>
        <td><?= User::getGroupName() ?></td>
        <td></td>
      </tr><tr>
        <td><button type="submit" name="action" value="confirm">Upravit</button></td>
        <td></td>
        <td>Pole označená (*) jsou povinná</td>
      </tr>
    </table>
  </form>

  <a href="/member/profil">Zpět</a>
</div>
