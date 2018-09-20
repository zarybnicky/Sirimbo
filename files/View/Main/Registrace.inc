<?= $this->header('Registrace') ?>

<div class="container">
  <form action="" method="POST">
    <table>
      <tr>
          <td>Uživatel (*): </td>
          <td><?= $this->text('username', $this->username) ?></td>
        <td>Pouze písmena bez diakritiky, číslice a podtržítka, 3 - 20 znaků</td>
      </tr><tr>
          <td>Heslo (*): </td>
          <td><input class="form-control" type="password" name="pass" value="<?= $this->pass ?>"/></td>
          <td>Pouze písmena bez diakritiky, číslice a podtržítka, 6 - 32 znaků</td>
      </tr><tr>
          <td>Jméno (*): </td>
          <td><?= $this->text('jmeno', $this->jmeno) ?></td>
          <td>Max. 40 znaků</td>
      </tr><tr>
          <td>Příjmení (*): </td>
          <td><?= $this->text('prijmeni', $this->prijmeni) ?></td>
          <td>Max. 40 znaků</td>
      </tr><tr>
          <td>Pohlaví (*): </td>
          <td>
              <?= $this->radio('pohlavi', 'm')->set($this->pohlavi == 'm')->label('Muž') ?>&nbsp;
              <?= $this->radio('pohlavi', 'f')->set($this->pohlavi == 'f')->label('Žena') ?>
          </td>
          <td></td>
      </tr><tr>
          <td>E-mail (*): </td>
          <td><?= $this->text('email', $this->email) ?></td>
          <td>Max. 50 znaků, ve formátu e-mail adresy</td>
      </tr><tr>
          <td>Telefon (*): </td>
          <td><?= $this->text('telefon', $this->telefon) ?></td>
          <td></td>
      </tr><tr>
          <td>Datum narození (*):</td>
          <td><?= $this->date('narozeni', $this->narozeni) ?></td>
          <td></td>
      </tr><tr>
          <td>Vztah ke klubu: </td>
          <td>
              <?= $this->radio('poznamky', 'dancer')
                       ->set($this->poznamky == 'dancer')
                       ->label('Tanečník/tanečníce') ?><br>
              <?= $this->radio('poznamky', 'parent')
                       ->set($this->poznamky == 'parent')
                       ->label('Rodič tanečníka') ?><br>
              <?= $this->radio('poznamky', 'other')
                       ->set($this->poznamky == 'other')
                       ->label('Jiné: ' . $this->text('other', $this->other)) ?>
          </td>
          <td></td>
      </tr><tr>
          <td><?= $this->submit('Registrovat se')->data('action', 'register') ?></td>
          <td>Pole označená (*) jsou povinná</td>
          <td></td>
      </tr>
    </table>
  </form>
</div>
