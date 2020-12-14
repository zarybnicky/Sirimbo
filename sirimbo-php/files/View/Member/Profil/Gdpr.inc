<form class="container" action="" method="POST">
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

  <?= (new \SubmitHelper('Souhlasím'))->data('action', 'gdpr') ?>
  <a class="btn btn-outline-primary" href="/logout">Odhlásit se</a>
</form>
