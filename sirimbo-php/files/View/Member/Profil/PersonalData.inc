<form class="container" action="" method="POST">
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

  <div class="form-check mb-2 pl-0">
    <?= new \CheckboxHelper('dancer', 'dancer', $dancer, 'Aktivní tanečník?') ?>
  </div>

  <div class="form-group mb-2">
    <?= new \HiddenHelper('returnURI', $returnURI) ?>
    <a class="btn btn-outline-primary" href="/member/profil">Zpět</a>
    <?= new \SubmitHelper('Upravit') ?>
  </div>
</form>
