<?= $this->header('Osobní údaje') ?>

<div class="container">
  <?php echo $this->notice($this->redirect()->getMessage());?>
  <form action="" method="POST">
    <table>
      <tr>
        <td>Uživatel: </td>
        <td><?php echo post("login"); ?></td>
        <td></td>
      </tr><tr>
        <td>Jméno: </td>
        <td><input type="text" name="jmeno" value="<?php echo post("jmeno"); ?>" /></td>
        <td>Max. 40 znaků</td>
      </tr><tr>
        <td>Příjmení: </td>
        <td><input type="text" name="prijmeni" value="<?php echo post("prijmeni"); ?>" /></td>
        <td>Max. 40 znaků</td>
      </tr><tr>
        <td>Pohlaví: </td>
        <td>
          <label><?php echo $this->radio('pohlavi', 'm');?>Muž</label>&nbsp;
          <label><?php echo $this->radio('pohlavi', 'f');?>Žena</label>
        </td>
        <td></td>
      </tr><tr>
        <td>E-mail (*): </td>
        <td><input type="text" name="email" value="<?php echo post("email"); ?>" /></td>
        <td>Max. 50 znaků, ve formátu e-mail adresy</td>
      </tr><tr>
        <td>Telefon (*): </td>
        <td><input type="text" name="telefon" value="<?php echo post("telefon"); ?>" /></td>
        <td></td>
      </tr><tr>
        <td>Datum narození: (*)</td>
        <td><?php echo $this->date('narozeni')->textBox(); ?></td>
        <td></td>
      </tr><tr>
        <td>Poznámky: </td>
        <td><textarea rows="15" cols="50" name="poznamky"><?php echo post("poznamky"); ?></textarea></td>
        <td></td>
      </tr><tr>
        <td>Oprávnění: </td>
        <td><?php echo User::getGroupName();?></td>
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
