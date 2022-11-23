import * as React from 'react';
import { CallToAction } from 'components/CallToAction';
import { SlateReadonly } from 'components/SlateReadonly';
import { Layout } from 'components/layout/Layout';
import { Heading } from 'components/Heading';
import { TrainerCard } from 'components/cards/TrainerCard';
import { Descendant } from 'slate';
import { ProspectForm } from 'components/ProspectForm';
import { LocationCard } from 'components/cards/LocationCard';

export default function AboutPage() {
  return <>
    <img src="https://tkolymp.cz/galerie/clanky/prijdtancit2.jpg" />

    <div className="col-feature mt-16 prose text-center">
      <h1>Kluci, přijďte si k nám pro taneční partnerku!</h1>
    </div>

    <div className="col-feature mt-16 grid lg:grid-cols-2">
      <div className="prose">
        <h2>Skupiny pro nové kluky</h2>

        <ul>
          <li>
            <b>Kluci 5 - 7 let</b>
            <ul>
              <li>Pondělí od 13:45 do 14:45</li>
              <li>Čtvrtek od 13:45 do 14:45</li>
            </ul>
          </li>

          <li>
            <b>Kluci 8 - 11 let</b>
            <ul>
              <li>Pondělí od 17:30 do 18:15</li>
              <li>Čtvrtek od 17:30 do 18:15</li>
            </ul>
          </li>

          <li>
            <b>Kluci 12 - 15 let</b>
            <ul>
              <li>Pondělí od 18:15 do 19:00</li>
              <li>Pátek od 15:30 do 16:15</li>
            </ul>
          </li>

          <li>
            <b>Kluci 16+</b>
            <ul>
              <li>Pondělí od 18:30 do 19:15</li>
              <li>Středa od 18:30 do 19:15</li>
            </ul>
          </li>
        </ul>

        <p>
          Kde: <b>Základní škola Holečkova 10</b>
        </p>
      </div>

      <div>
        ?????????
      </div>
    </div>

    <div className="my-8 grid lg:grid-cols-2">
      <div className="prose">
        <h2>Na co se můžete těšit?</h2>
        <ul>
          <li>Výukové lekce standardních a latinskoamerických tanců</li>
          <li>Taneční partnerka, která již zná základní kroky</li>
          <li>Můžete se přijít podívat na kteroukoli hodinu, stačí se přihlásit trenérovi před začátkem hodiny.</li>
          <li>První hodina <b>ZDARMA</b> po nezávazném přihlášení</li>
          <li>Cena <b>800 Kč</b> za listopad - leden (namísto 2400 Kč)</li>
          <li>
            Startovací soustředění 10. 12. 2022 <b>ZDARMA</b>
            <ul>
              <li>ZŠ Holečkova</li>
              <li>seznámení s novým prostředím a novými kamarády</li>
              <li>uvedení do tanečnního sportu zábavnou formou</li>
              <li>od 8:30 do 14:30</li>
            </ul>
          </li>
        </ul>
      </div>

      <div className="prose">
        <h2>Víte, že...?</h2>
        <ul>
          <li>Taneční sport je nově olympijským sportem</li>
          <li>Sportovcům přináší řadu možností a skvělých zážitků</li>
          <li>Většina účinkujících ve StarDance se věnovalo tanečnímu sportu</li>
          <li>Naučí děti základy timemanagentu - zorganizovat školu, trénink\\na ostatní aktivity</li>
          <li>Tanečníci se mohou stát členy národního reprezentačního týmu\\na reprezentovat tak ČR v zahraničí</li>
        </ul>
      </div>
    </div>

    <div className="my-8 grid lg:grid-cols-2">
      <div className="prose">
        <h2>Informace k tréninku</h2>
        <ul>
          <li>Hodina trvá 45 minut, přijďte 10-15 minut předem, k dispozici je špatna pro převléknutí</li>
          <li>Trenéři: Miroslav Hýža a Hana Anna Šišková</li>
          <li>Náplň: pohybové hry, základy tanečních kroků, základy posilování</li>
          <li>Oblečení - volné sportovní, sálová obuv (sálové tenisky do tělocviku)</li>
          <li>Přítomnost rodičů se nedoporučuje</li>
        </ul>
      </div>
      <div className="prose">
        <img src="https://tkolymp.cz/galerie/clanky/Týmové-foto-1.jpg" />
      </div>
    </div>

    <div className="my-8 grid lg:grid-cols-2">
      <ProspectForm title="Zapiš se na první hodinu ZDARMA!" />

      <div className="prose">
        <h2>Co vás čeká...?</h2>
        <ul>
          <li>Základní kroky čtyř tanců (Waltz, Polka, Chacha, Jive)</li>
          <li>Rozvíjení tělesné zdatnosti a správného držení těla</li>
          <li>Rozvíjení správných pohybových návyků</li>
          <li>Trenér národního reprezentačního týmu (každý týden)</li>
          <li>Pěstování návyků Fair Play</li>
          <li>Pravidelná jednodenní víkendová soustředění</li>
          <li>Doprovodné akce: Vánoční besídka s odměnou (prosinec)</li>
          <li>Vystoupení na Akademii Olymp Dance (leden), Rodinné vstupné na taneční soutěž pro celou rodinu zdarma (květen)</li>
        </ul>
      </div>
    </div>

    <div className="my-8 grid lg:grid-cols-2">
      https://www.youtube.com/watch?v=MSahpLevS2g

      <div className="prose">
        <h3>Znáte StarDance? A připadá vám někdy neuvěřitelné naučit se těžké kroky do velkého množství tanců?</h3>
        <h3>My to Vaše děti umíme naučit! Umíme z nich vychovat vrcholové sportovce!</h3>
      </div>
    </div>

    <div className="my-8 grid lg:grid-cols-2">
      <div className="prose">
        <h2>Příběh tanečníků</h2>
        <p>Dan s Barčou spolu začali tancovat v roce 2010 v dětské věkové kategorii. V roce 2014 poprvé startovali na mistrovství České republiky, již o 2 roky později zvítězili ve všech třech disciplínách. V následujících letech se díky členství v národním reprezentačním týmu dostali na řadu zahraničních soutěží, reprezentovali ČR na několika mistrovstvích světa, například ve španělském Bilbau. Během taneční kariéry se dostali na tréninková soustředění do Itálie, zúčastnili se řady zahraničních soutěží ve Francii, Itálii a v dalších zemích.</p>
      </div>

      https://www.youtube.com/watch?v=lURCOEiVbGc
    </div>

    <div className="prose text-center">
      <h2>Nečekejte, až vaše děti vyrostou, vrcholoví sportovci začínají již v dětském věku.</h2>
    </div>

    <div className="my-8 grid lg:grid-cols-2">
      <img src="https://tkolymp.cz/galerie/clanky/Druzstva2019STTgroupmakrlik.jpg" />

      <div className="relative aspect-w-16 aspect-h-9">
        <div className="absolute inset-0 z-10" />
        <iframe frameBorder="0" allowFullScreen allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture" title="Vánoční večírek Olymp DANCE 2021" width="100%" height="100%" src="https://www.youtube.com/embed/TOrHc7JYUac?autoplay=0&amp;mute=0&amp;controls=0&amp;origin=https%3A%2F%2Folymp.zarybnicky.com&amp;playsinline=1&amp;showinfo=0&amp;rel=0&amp;iv_load_policy=3&amp;modestbranding=1&amp;enablejsapi=1&amp;widgetid=23"></iframe>
      </div>
    </div>

    <ProspectForm title="Zapiš se na první hodinu ZDARMA!" />

    <img src="https://tkolymp.cz/galerie/clanky/TKOLYMP-nabor-FB-post-1200x630.png" />

    <LocationCard item={{
      image: "",
      name: "Taneční centrum při FZŠ Holečkova",
      address: "Holečkova 10, 779 00, Olomouc (vchod brankou u zastávy Povel - škola)",
      href: "https://www.zsholeckova.cz/",
      mapHref: "https://goo.gl/maps/swv3trZB2uvjcQfR6",
      map: {
        lat: 49.57963,
        lng: 17.2495939,
        zoom: 12,
      },
    }} />

    <CallToAction />
  </>;
};

AboutPage.getLayout = (page: React.ReactElement) => <Layout showTopMenu>{page}</Layout>;
