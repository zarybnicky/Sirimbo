<form class="container" action="" method="POST">

  <div class="form-row mb-2">
    <div class="form-group col-12 col-md-6">
      <label for="username">Přihlašovací jméno</label>
      <?= $this->text('username', $this->username) ?>
      <small class="form-text text-muted">
        Pouze písmena bez diakritiky, číslice a podtržítka, 3 - 20 znaků
      </small>
    </div>
    <div class="form-group col-12 col-md-6">
      <label for="pass">Heslo</label>
      <input class="form-control" type="password" name="pass" value="<?= $this->pass ?>"/>
      <small class="form-text text-muted">
        Pouze písmena bez diakritiky, číslice a podtržítka, 6 - 32 znaků
      </small>
    </div>
  </div>

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

  <div class="form-group mb-2">
    <label for="skupina">Tréninková skupina</label>
    <?php
    $s = $this->select('skupina')->set($this->skupina ?: '3');
    foreach ($this->skupiny as $item) {
        $s->option(
            $item['id'],
            new ColorboxHelper($item['color'], $item['popis']) . '&nbsp;' . $item['popis']
        );
    }
    echo $s;
    ?>
  </div>

  <div class="form-group mb-2">
    <label for="poznamky">Vztah ke klubu</label>
    <div class="form-inline">
      <?= $this->radio('poznamky', 'dancer')
             ->set($this->poznamky == 'dancer')
             ->label('Tanečník/tanečníce') ?>
    </div>
    <div class="form-inline">
      <?php $i = $this->text('dancer-name', $this->dancerName)
                     ->placeholder('jméno tanečníka')
                     ->cls('form-control ml-2') ?>
      <?= $this->radio('poznamky', 'parent')
             ->set($this->poznamky == 'parent')
             ->label('Rodič tanečníka: ' . $i) ?>
    </div>
    <div class="form-inline">
      <?php $i = $this->text('other', $this->other)
                     ->placeholder('vztah ke klubu')
                     ->cls('form-control ml-2') ?>
      <?= $this->radio('poznamky', 'other')
             ->set($this->poznamky == 'other')
             ->label('Jiný ' . $i) ?>
    </div>
  </div>

  <p class="pt-2">
    Zákon č. 101/2000 Sb., o ochraně osobních údajů, ve znění pozdějších předpisů, ukládá
    <b>Tanečnímu klubu Olymp Olomouc, z. s., IČ: 68347286, se sídlem: Jiráskova 381/25, Olomouc</b>
    (dále jen „klub“) práva a povinnosti, mezi něž mimo jiné patří i povinnost informovat své členy
    o právech, které se týkají ochrany osobních údajů člena, respektive budoucího člena o přístupu
    k osobním údajům, zpracování a předávání osobních údajů třetí osobě.
  </p><p>
    Stvrzuji, že tuto povinnost klub splnil a souhlasím, aby klub shromažďoval a
    zpracovával mé (mého syna/dcery) osobní údaje v souladu s právy a povinnostmi, které mu
    ukládají obecně závazné právní předpisy.
  </p><p>
    Dávám tímto výslovný souhlas s použitím citlivého údaje – fotografie – na webu klubu.
    Souhlasím s používáním všech fotografií a videí ze soutěží, tréninků, soustředění… vytvořených
    pro účely propagace klubu. Souhlasím s jejich zveřejněním na webových stránkách klubu a
    sociálních sítích Facebook a Instagram.
  </p><p>
    Dojde-li ke změně v mých osobních údajích, zavazuji se je klubu oznámit neprodleně. Můj
    souhlas se zpracováním osobních údajů se bude vztahovat i na tyto nově oznámené osobní
    údaje.
  </p>

  <?= $this->submit('Registrovat se')->data('action', 'register') ?>
</form>
