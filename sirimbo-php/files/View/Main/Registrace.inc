<form class="container" action="" method="POST">

  <div class="form-row mb-2">
    <div class="form-group col-12 col-md-6">
      <label for="username">Přihlašovací jméno</label>
      <?= \Utils::text('username', $username) ?>
      <small class="form-text text-muted">
        Pouze písmena bez diakritiky, číslice a podtržítka, 3 - 20 znaků
      </small>
    </div>
    <div class="form-group col-12 col-md-6">
      <label for="pass">Heslo</label>
      <input class="form-control" type="password" name="pass" value="<?= $pass ?>"/>
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
    <label for="skupina">Tréninková skupina</label>
    <?= \Utils::selectAssoc('skupina', $skupiny, 'id', 'text', $skupina ?: '3') ?>
  </div>

  <div class="form-group mb-2">
    <label for="poznamky">Vztah ke klubu</label>
    <div class="form-inline">
      <?= \Utils::radio('poznamky', 'dancer', $poznamky == 'dancer', 'Tanečník/tanečníce') ?>
    </div>
    <div class="form-inline">
      <?php $i = \Utils::text('dancer-name', $dancerName, [
          'placeholder' => 'jméno tanečníka',
          'cls' => 'form-control ml-2',
      ]); ?>
      <?= \Utils::radio('poznamky', 'parent', $poznamky == 'parent', 'Rodič tanečníka: ' . $i) ?>
    </div>
    <div class="form-inline">
      <?php $i = \Utils::text('other', $other, [
          'placeholder' => 'vztah ke klubu',
          'cls' => 'form-control ml-2',
      ]) ?>
      <?= \Utils::radio('poznamky', 'other', $poznamky == 'other', 'Jiný ' . $i) ?>
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

  <?= \Utils::submit('Registrovat se', 'action', 'register') ?>
</form>
