import Map from '@app/map';
import { ProspectForm } from '@app/ui/ProspectForm';
import { YoutubeEmbed } from '@app/ui/YoutubeEmbed';
import { Layout } from 'components/layout/Layout';
import { NextSeo } from 'next-seo';
import * as React from 'react';

const Page = () => {
  const scrollToForm = (e: React.MouseEvent<HTMLAnchorElement>) => {
    e.preventDefault();
    document.querySelector(e.currentTarget.getAttribute('href') || '#form')?.scrollIntoView({
      behavior: 'smooth'
    });
  };
  return (
    <Layout showTopMenu>
      <NextSeo title="Přijď tančit!" />
      <div className="col-feature my-8">
        <img alt="" src="https://files.rozpisovnik.cz/file/rozpisovnik/tkolymp/1693388158598-TKOLYMP-nabor-INSTG-post-1080x560.png" />
      </div>

      <div className="col-feature my-8 grid lg:grid-cols-2 gap-4">
        <div className="prose prose-accent">
          <h2>Na co se můžete těšit?</h2>
          <ul>
            <li>Výukové lekce standardních a latinskoamerických tanců dle věku</li>
            <li>Můžete se přijít podívat na kteroukoli hodinu, stačí se přihlásit trenérovi před začátkem hodiny</li>
            <li>První hodina ZDARMA po nezávazném přihlášení</li>
            <li>Cena 1400 Kč za čtvrtletí, 2600 Kč při pololetní platbě</li>
            <li>Výukové lekce standardních a latinskoamerických tanců</li>
          </ul>
        </div>

        <div className="prose prose-accent">
          <h2>Ukázkové hodiny v září 2023</h2>
          <ul>
            <li><b>pondělí 18.9. v 17:30</b></li>
            <li><b>čtvrtek 21.9. v 17:30</b></li>
          </ul>
          <h3>Kde nás najdete?</h3>
          <p>
            Taneční centrum při FZŠ Holečkova<br />
            Holečkova 10, 779 00, Olomouc<br />
            (vchod brankou u zastávky Povel - škola)
          </p>
        </div>
      </div>

      <div className="col-feature my-8 grid lg:grid-cols-2 gap-4">
        <div className="prose prose-accent">
          <h2>Víte, že...?</h2>
          <ul>
            <li>Taneční sport je nově olympijským sportem</li>
            <li>Sportovcům přináší řadu možností a skvělých zážitků</li>
            <li>Většina účinkujících ve StarDance se věnovalo tanečnímu sportu</li>
            <li>
              Naučí děti základy time managentu - zorganizovat školu, trénink na ostatní
              aktivity
            </li>
            <li>
              Tanečníci se mohou stát členy národního reprezentačního týmu na
              reprezentovat tak ČR v zahraničí
            </li>
          </ul>
        </div>

        <img alt="" src="https://tkolymp.cz/galerie/clanky/Týmové-foto-1.jpg" />
      </div>

      <YoutubeEmbed
        title=""
        thumbnail="https://i3.ytimg.com/vi/MSahpLevS2g/maxresdefault.jpg"
      >
        <iframe
          allowFullScreen
          allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture"
          width="100%"
          height="100%"
          src="https://www.youtube.com/embed/MSahpLevS2g?autoplay=1&amp;mute=0&amp;controls=1&amp;origin=https%3A%2F%2Ftkolymp.cz&amp;playsinline=1&amp;showinfo=0&amp;rel=0&amp;iv_load_policy=3&amp;modestbranding=1&amp;enablejsapi=1&amp;widgetid=23"
        ></iframe>
      </YoutubeEmbed>

      <div className="col-feature my-16 grid lg:grid-cols-2 gap-4">
        <div className="prose prose-accent">
          <h2>Jak vypadá taneční trénink?</h2>
          <ul>
            <li>
              Hodina trvá 45 minut, přijďte 10-15 minut předem, k dispozici je špatna pro
              převléknutí
            </li>
            <li>Trenéři: Miroslav Hýža a Hana Anna Šišková</li>
            <li>Náplň: pohybové hry, základy tanečních kroků, základy posilování</li>
            <li>Oblečení - volné sportovní, sálová obuv (sálové tenisky do tělocviku)</li>
            <li>Přítomnost rodičů se nedoporučuje</li>
          </ul>
        </div>

        <div className="prose prose-accent">
          <h2>Co vás čeká...?</h2>
          <ul>
            <li>Základní kroky čtyř tanců (Waltz, Polka, Chacha, Jive)</li>
            <li>Rozvíjení tělesné zdatnosti a správného držení těla</li>
            <li>Rozvíjení správných pohybových návyků</li>
            <li>Trenér národního reprezentačního týmu (každý týden)</li>
            <li>Pěstování návyků Fair Play</li>
            <li>Pravidelná jednodenní víkendová soustředění</li>
            <li>Doprovodné akce: Vánoční besídka s odměnou (prosinec)</li>
            <li>
              Vystoupení na Akademii Olymp Dance (leden), Rodinné vstupné na taneční
              soutěž pro celou rodinu zdarma (květen)
            </li>
          </ul>
        </div>
      </div>

      <div className="text-4xl text-accent-0 text-center font-bold rounded-2xl p-4 shadow-md bg-accent-9 hover:bg-accent-10">
        <a href="#form" onClick={scrollToForm}>
          <h1>ZAPIŠ SE NA PRVNÍ HODINU ZDARMA!</h1>
        </a>
      </div>

      <div className="col-feature my-16 grid lg:grid-cols-2 gap-4">
        <YoutubeEmbed
          title=""
          thumbnail="https://i3.ytimg.com/vi/flkU9ZeM7_8/maxresdefault.jpg"
        >
          <iframe
            allowFullScreen
            allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture"
            width="100%"
            height="100%"
            src="https://www.youtube.com/embed/flkU9ZeM7_8?autoplay=1&amp;mute=0&amp;controls=1&amp;origin=https%3A%2F%2Ftkolymp.cz&amp;playsinline=1&amp;showinfo=0&amp;rel=0&amp;iv_load_policy=3&amp;modestbranding=1&amp;enablejsapi=1&amp;widgetid=23"
          ></iframe>
        </YoutubeEmbed>

        <div className="prose prose-accent self-center">
          <h3>
            Znáte StarDance? A připadá vám někdy neuvěřitelné naučit se těžké kroky do
            velkého množství tanců?
          </h3>
          <h3>
            My to Vaše děti umíme naučit! Umíme z nich vychovat vrcholové sportovce!
          </h3>
        </div>
      </div>

      <div className="col-feature my-8 grid lg:grid-cols-2 items-center gap-4">
        <div className="prose prose-accent">
          <h2>Příběh tanečníků</h2>
          <p>Dan s Barčou spolu začali tancovat v roce 2010 v dětské věkové kategorii.</p>
          <p>
            V roce 2014 poprvé startovali na mistrovství České republiky, již o 2 roky
            později zvítězili ve všech třech disciplínách.
          </p>
          <p>
            V následujících letech se díky členství v národním reprezentačním týmu dostali
            na řadu zahraničních soutěží, reprezentovali ČR na několika mistrovstvích
            světa, například ve španělském Bilbau.
          </p>
          <p>
            Během taneční kariéry se dostali na tréninková soustředění do Itálie,
            zúčastnili se řady zahraničních soutěží ve Francii, Itálii a v dalších zemích.
          </p>
        </div>

        <YoutubeEmbed
          title=""
          thumbnail="https://i.ytimg.com/vi/lURCOEiVbGc/maxres1.jpg"
        >
          <iframe
            allowFullScreen
            allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture"
            width="100%"
            height="100%"
            src="https://www.youtube.com/embed/lURCOEiVbGc?autoplay=1&amp;mute=0&amp;controls=1&amp;origin=https%3A%2F%2Ftkolymp.cz&amp;playsinline=1&amp;showinfo=0&amp;rel=0&amp;iv_load_policy=3&amp;modestbranding=1&amp;enablejsapi=1&amp;widgetid=23"
          ></iframe>
        </YoutubeEmbed>
      </div>

      <div className="my-8 prose prose-accent text-center">
        <h2>
          Nečekejte, až vaše děti vyrostou, vrcholoví sportovci začínají již v dětském
          věku.
        </h2>
      </div>

      <div className="col-feature my-8 grid lg:grid-cols-2 items-center gap-4">
        <img alt="" src="https://tkolymp.cz/galerie/clanky/Druzstva2019STTgroupmakrlik.jpg" />

        <YoutubeEmbed
          title=""
          thumbnail="https://i3.ytimg.com/vi/TOrHc7JYUac/maxresdefault.jpg"
        >
          <iframe
            allowFullScreen
            allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture"
            title="Vánoční večírek Olymp DANCE 2021"
            width="100%"
            height="100%"
            src="https://www.youtube.com/embed/TOrHc7JYUac?autoplay=1&amp;mute=0&amp;controls=0&amp;origin=https%3A%2F%2Folymp.zarybnicky.com&amp;playsinline=1&amp;showinfo=0&amp;rel=0&amp;iv_load_policy=3&amp;modestbranding=1&amp;enablejsapi=1&amp;widgetid=23"
          ></iframe>
        </YoutubeEmbed>
      </div>

      <div id="form">
        <ProspectForm title="Zapiš se na první hodinu ZDARMA!" />
      </div>

      <div className="col-feature my-16 grid lg:grid-cols-2 gap-4">
        <Map className="min-h-48 min-w-24" center={{ lat: 49.57963, lng: 17.2495939 }} zoom={12} scrollWheelZoom={false}>
          {({ TileLayer, Marker, Popup }) => (
            <>
              <TileLayer url="https://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png" />
              <Marker position={{ lat: 49.57963, lng: 17.2495939 }}>
                <Popup>ZŠ Holečkova</Popup>
              </Marker>
            </>
          )}
        </Map>

        <div className="prose prose-accent py-8">
          <h3>Taneční centrum při FZŠ Holečkova</h3>
          <p>
            Holečkova 10, 779 00, Olomouc<br />
            (vchod brankou u zastávy Povel - škola)
          </p>
          <a href="https://www.zsholeckova.cz/" target="_blank">https://www.zsholeckova.cz/</a>
          <a href="https://goo.gl/maps/swv3trZB2uvjcQfR6" target="_blank">Otevřít mapu</a>
        </div>
      </div>
    </Layout>
  );
};

export default Page;
