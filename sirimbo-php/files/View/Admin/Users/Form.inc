<form class="container" action="" method="POST">
  <div class="form-row mb-2">
    <div class="form-group col-12 col-md-6">
      <label for="username">Přihlašovací jméno</label>
      <?= (new \TextHelper('login', $login))->readonly($action == 'edit') ?>
      <small class="form-text text-muted">
        Pouze písmena bez diakritiky, číslice a podtržítka, 3 - 20 znaků
      </small>
    </div>
    <div class="form-group col-12 col-md-6">
      <label for="pass">Heslo</label>
      <?= (new \TextHelper('pass', $pass))->disabled($action == 'edit') ?>
      <small class="form-text text-muted">
        Pouze písmena bez diakritiky, číslice a podtržítka, 6 - 32 znaků
      </small>
    </div>
  </div>

  <div class="form-row mb-2">
    <div class="form-group col-12 col-md-6">
      <label for="jmeno">Jméno</label>
      <?= new \TextHelper('jmeno', $jmeno) ?>
    </div>
    <div class="form-group col-12 col-md-6">
      <label for="prijmeni">Příjmení</label>
      <?= new \TextHelper('prijmeni', $prijmeni) ?>
    </div>
  </div>

  <div class="form-row mb-2">
    <div class="form-group col-12 col-md-6">
      <label for="narozeni">Datum narození</label>
      <?= new \DateHelper('narozeni', $narozeni) ?>
    </div>
    <div class="form-group col-12 col-md-6 pt-4">
      <?= (new \RadioHelper('pohlavi', 'm', $pohlavi == 'm'))->label('Muž')->inline() ?>
      <?= (new \RadioHelper('pohlavi', 'f', $pohlavi == 'f'))->label('Žena')->inline() ?>
    </div>
  </div>

  <div class="form-row mb-2">
    <div class="form-group col-12 col-md-6">
      <label for="email">E-mail</label>
      <?= new \TextHelper('email', $email) ?>
    </div>
    <div class="form-group col-12 col-md-6">
      <label for="telefon">Telefon</label>
      <?= new \TextHelper('telefon', $telefon) ?>
    </div>
  </div>

  <div class="mt-3 mb-1 text-muted">Bydliště</div>
  <div class="form-row mb-2">
    <div class="form-group mb-2 col-12 col-md-6">
      <label for="street">Ulice</label>
      <?= new \TextHelper('street', $street) ?>
    </div>
    <div class="form-group col-6 col-md-3">
      <label for="popisne">Č. popisné</label>
      <?= new \TextHelper('popisne', $popisne) ?>
    </div>
    <div class="form-group col-6 col-md-3">
      <label for="orientacni">Č. orientační</label>
      <?= new \TextHelper('orientacni', $orientacni) ?>
    </div>

    <div class="form-group mb-2 col-12 col-md-6">
      <label for="city">Město</label>
      <?= new \TextHelper('city', $city) ?>
    </div>
    <div class="form-group col-12 col-md-6">
      <label for="district">Část města</label>
      <?= new \TextHelper('district', $district) ?>
    </div>

    <div class="form-group col-12 col-md-6">
      <label for="postal">PSČ</label>
      <?= new \TextHelper('postal', $postal) ?>
    </div>
  </div>

  <div class="form-group mb-2">
    <label for="nationality">Národnost</label>
    <?= new \SelectHelper('nationality', Countries::$countries, $nationality) ?>
  </div>

  <div class="form-group mb-2">
    <label for="poznamky">Poznámky</label>
    <textarea class="form-control" name="poznamky" rows="15"><?= $poznamky ?></textarea>
  </div>
  <div class="form-check mb-2">
    <?= new \CheckboxHelper('dancer', 'dancer', $dancer, 'Aktivní tanečník') ?>
  </div>
  <div class="form-check mb-2">
    <?= new \CheckboxHelper('teacher', 'teacher', $teacher, 'Trenér') ?>
  </div>
  <div class="form-check mb-2">
    <?= new \CheckboxHelper('lock', 'lock', $lock, 'Uzamknout') ?>
  </div>
  <div class="form-check mb-2">
    <?= new \CheckboxHelper('ban', 'ban', $ban, 'Ban') ?>
  </div>
  <div class="form-check mb-2">
    <?= new \CheckboxHelper('system', 'system', $system, 'Systémový uživatel') ?>
  </div>
  <div class="form-group mb-2">
    <label for="group">Oprávnění</label>
    <?php
    $s = new \SelectHelper('group', [], $group);
    foreach ($groups as $item) {
        $s->option($item['id'], $item['name']);
    }
    echo $s;
    ?>
  </div>
  <div class="form-group mb-2">
    <div style="font-weight:bold">Skupina</div>
    <?php
    foreach ($skupiny as $item) {
        echo (new \RadioHelper('skupina', $item['id'], $skupina == $item['id']))
                 ->label(new ColorboxHelper($item['color'], $item['popis']) . '&nbsp;' . $item['popis']);
    }?>
  </div>
  <div class="mb-2">
      Uživatel vytvořen: <?= $createdAt ?><br>
      Uživatel souhlasil s GDPR: <?= $gdprSignedAt ?>
  </div>
  <div class="form-group">
    <?= new \HiddenHelper('returnURI', $returnURI) ?>
    <a class="btn btn-outline-primary" href="<?= $returnURI ?>">Zpět</a>
    <?= new \SubmitHelper($action == 'add' ? 'Přidat' : 'Upravit') ?>
  </div>
</form>
