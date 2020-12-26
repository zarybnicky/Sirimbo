<form class="container" action="" method="POST">
  <div class="form-row mb-2">
    <div class="form-group col-12 col-md-6">
      <label for="username">Přihlašovací jméno</label>
      <?= \Utils::text('login', $login, ['readonly' => $action == 'edit']) ?>
      <small class="form-text text-muted">
        Pouze písmena bez diakritiky, číslice a podtržítka, 3 - 20 znaků
      </small>
    </div>
    <div class="form-group col-12 col-md-6">
      <label for="pass">Heslo</label>
      <?= \Utils::text('pass', $pass, ['disabled' => $action == 'edit']) ?>
      <small class="form-text text-muted">
        Pouze písmena bez diakritiky, číslice a podtržítka, 6 - 32 znaků
      </small>
    </div>
  </div>

  <div class="form-row mb-2">
    <div class="form-group col-12 col-md-6">
      <label for="jmeno">Jméno</label>
      <?= \Utils::text('jmeno', $jmeno) ?>
    </div>
    <div class="form-group col-12 col-md-6">
      <label for="prijmeni">Příjmení</label>
      <?= \Utils::text('prijmeni', $prijmeni) ?>
    </div>
  </div>

  <div class="form-row mb-2">
    <div class="form-group col-12 col-md-6">
      <label for="narozeni">Datum narození</label>
      <?= new \DateHelper('narozeni', $narozeni) ?>
    </div>
    <div class="form-group col-12 col-md-6 pt-4">
      <?= \Utils::inlineRadio('pohlavi', 'm', $pohlavi == 'm', 'Muž') ?>
      <?= \Utils::inlineRadio('pohlavi', 'f', $pohlavi == 'f', 'Žena') ?>
    </div>
  </div>

  <div class="form-row mb-2">
    <div class="form-group col-12 col-md-6">
      <label for="email">E-mail</label>
      <?= \Utils::text('email', $email) ?>
    </div>
    <div class="form-group col-12 col-md-6">
      <label for="telefon">Telefon</label>
      <?= \Utils::text('telefon', $telefon) ?>
    </div>
  </div>

  <div class="mt-3 mb-1 text-muted">Bydliště</div>
  <div class="form-row mb-2">
    <div class="form-group mb-2 col-12 col-md-6">
      <label for="street">Ulice</label>
      <?= \Utils::text('street', $street) ?>
    </div>
    <div class="form-group col-6 col-md-3">
      <label for="popisne">Č. popisné</label>
      <?= \Utils::text('popisne', $popisne) ?>
    </div>
    <div class="form-group col-6 col-md-3">
      <label for="orientacni">Č. orientační</label>
      <?= \Utils::text('orientacni', $orientacni) ?>
    </div>

    <div class="form-group mb-2 col-12 col-md-6">
      <label for="city">Město</label>
      <?= \Utils::text('city', $city) ?>
    </div>
    <div class="form-group col-12 col-md-6">
      <label for="district">Část města</label>
      <?= \Utils::text('district', $district) ?>
    </div>

    <div class="form-group col-12 col-md-6">
      <label for="postal">PSČ</label>
      <?= \Utils::text('postal', $postal) ?>
    </div>
  </div>

  <div class="form-group mb-2">
    <label for="nationality">Národnost</label>
    <?= \Utils::select('nationality', Countries::$countries, $nationality) ?>
  </div>

  <div class="form-group mb-2">
    <label for="poznamky">Poznámky</label>
    <textarea class="form-control" name="poznamky" rows="15"><?= $poznamky ?></textarea>
  </div>
  <div class="form-check mb-2">
    <?= \Utils::checkbox('dancer', 'dancer', $dancer, 'Aktivní tanečník') ?>
  </div>
  <div class="form-check mb-2">
    <?= \Utils::checkbox('teacher', 'teacher', $teacher, 'Trenér') ?>
  </div>
  <div class="form-check mb-2">
    <?= \Utils::checkbox('lock', 'lock', $lock, 'Uzamknout') ?>
  </div>
  <div class="form-check mb-2">
    <?= \Utils::checkbox('ban', 'ban', $ban, 'Ban') ?>
  </div>
  <div class="form-check mb-2">
    <?= \Utils::checkbox('system', 'system', $system, 'Systémový uživatel') ?>
  </div>
  <div class="form-group mb-2">
    <label for="group">Oprávnění</label>
    <?= \Utils::selectAssoc('group', $groups, 'id', 'name', $group) ?>
  </div>
  <div class="form-group mb-2">
    <div style="font-weight:bold">Skupina</div>
    <?php foreach ($skupiny as $item) : ?>
      <?= \Utils::radio(
          'skupina',
          $item['id'],
          $skupina == $item['id'],
          "<div class=\"box\" title=\"{$item['popis']}\" style=\"background-color:{$item['color']}\"></div>"
          . '&nbsp;' . $item['popis']
      ) ?>
    <?php endforeach ?>
  </div>
  <div class="mb-2">
      Uživatel vytvořen: <?= $createdAt ?><br>
      Uživatel souhlasil s GDPR: <?= $gdprSignedAt ?>
  </div>
  <div class="form-group">
    <input type='hidden' name='returnURI' value='<?= $returnURI ?>'>
    <a class="btn btn-outline-primary" href="<?= $returnURI ?>">Zpět</a>
    <?= \Utils::submit($action == 'add' ? 'Přidat' : 'Upravit') ?>
  </div>
</form>
