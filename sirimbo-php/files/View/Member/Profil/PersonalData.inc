<form class="container" action="" method="POST">
  <div class="form-row mb-2">
    <div class="form-group col-12 col-md-6">
      <label for="jmeno">Jméno</label>
      <?= $this->text('jmeno', $this->jmeno) ?>
    </div>
    <div class="form-group col-12 col-md-6">
      <label for="prijmeni">Příjmení</label>
      <?= $this->text('prijmeni', $this->prijmeni) ?>
    </div>
  </div>

  <div class="form-row mb-2">
    <div class="form-group col-12 col-md-6">
      <label for="narozeni">Datum narození</label>
      <?= $this->date('narozeni', $this->narozeni) ?>
    </div>
    <div class="form-group col-12 col-md-6 pt-4">
      <?= $this->radio('pohlavi', 'm')->set($this->pohlavi == 'm')->label('Muž')->inline() ?>
      <?= $this->radio('pohlavi', 'f')->set($this->pohlavi == 'f')->label('Žena')->inline() ?>
    </div>
  </div>

  <div class="form-row mb-2">
    <div class="form-group col-12 col-md-6">
      <label for="email">E-mail</label>
      <?= $this->text('email', $this->email) ?>
    </div>
    <div class="form-group col-12 col-md-6">
      <label for="telefon">Telefon</label>
      <?= $this->text('telefon', $this->telefon) ?>
    </div>
  </div>

  <div class="mt-3 mb-1 text-muted">Bydliště</div>
  <div class="form-row mb-2">
    <div class="form-group mb-2 col-12 col-md-6">
      <label for="street">Ulice</label>
      <?= $this->text('street', $this->street) ?>
    </div>
    <div class="form-group col-6 col-md-3">
      <label for="popisne">Č. popisné</label>
      <?= $this->text('popisne', $this->popisne) ?>
    </div>
    <div class="form-group col-6 col-md-3">
      <label for="orientacni">Č. orientační</label>
      <?= $this->text('orientacni', $this->orientacni) ?>
    </div>

    <div class="form-group mb-2 col-12 col-md-6">
      <label for="city">Město</label>
      <?= $this->text('city', $this->city) ?>
    </div>
    <div class="form-group col-12 col-md-6">
      <label for="district">Část města</label>
      <?= $this->text('district', $this->district) ?>
    </div>

    <div class="form-group col-12 col-md-6">
      <label for="postal">PSČ</label>
      <?= $this->text('postal', $this->postal) ?>
    </div>
  </div>

  <div class="form-group mb-2">
    <label for="nationality">Národnost</label>
    <?= $this->select('nationality')
           ->options(Countries::$countries)
           ->set($this->nationality) ?>
  </div>

  <div class="form-check mb-2 pl-0">
    <?= $this->checkbox('dancer', 'dancer')->label('Aktivní tanečník?')->set($this->dancer) ?>
  </div>

  <div class="form-group mb-2">
    <?= $this->hidden('returnURI', $this->returnURI) ?>
    <a class="btn btn-outline-primary" href="/member/profil">Zpět</a>
    <?= $this->submit('Upravit') ?>
  </div>
</form>
