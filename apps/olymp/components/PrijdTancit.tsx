import { CallToAction } from './CallToAction';
import { ProspectForm } from '@app/ui/ProspectForm';
import { YoutubeEmbed } from '@app/ui/YoutubeEmbed';
import Map from '@app/map'
import * as React from 'react';

export default function PrijdTancit() {
  return (
    <div className="content relative">
      <h1 className="mt-12 mb-8 text-3xl text-primary drop-shadow tracking-wide">
        Tanec není sport pro každého…<br />
        …ale Vaše dítě ho bude milovat!
      </h1>

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

        <div className="prose prose-accent">
          <h2>Skupiny pro nováčky do června 2023</h2>
          <h3>5 - 7 let</h3>
          <ul>
            <li>Pondělí od 13:45 do 14:45</li>
            <li>Čtvrtek od 13:45 do 14:45</li>
          </ul>

          <h3>8 - 11 let</h3>
          <ul>
            <li>Pondělí od 17:30 do 18:15</li>
            <li>Čtvrtek od 17:30 do 18:1512 - 15 let</li>
            <li>Pondělí od 18:15 do 19:00</li>
            <li>Pátek od 15.30 do 16:15</li>
          </ul>

          <h3>16+</h3>
          <ul>
            <li>Pondělí 18:30 - 19:15</li>
            <li>Středa 18:30 - 19:15</li>
          </ul>

          <p>Časy jednotlivých skupin se mohou od září 2023 změnit. Vyplňte nezávazný formulář níže a my Vám zašleme bližší informace.</p>
        </div>
      </div>

      <img alt="" src="https://tkolymp.cz/galerie/clanky/Týmové-foto-1.jpg" />

      <div className="col-feature my-16 grid lg:grid-cols-2 gap-4">
        <div className="prose prose-accent">
          <h2>Informace k tréninku</h2>
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

      <ProspectForm title="Zapiš se na první hodinu ZDARMA!" />

      <div className="col-feature my-16 grid lg:grid-cols-2 gap-4">
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

      <div className="col-feature my-8 grid lg:grid-cols-2 gap-4">
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
          thumbnail="https://i.ytimg.com/vi/lURCOEiVbGc/hqdefault.jpg"
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

      <div className="col-feature my-8 grid lg:grid-cols-2 gap-4">
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

      <ProspectForm title="Zapiš se na první hodinu ZDARMA!" />

      <div className="col-feature my-8">
        <img alt="" src="https://tkolymp.cz/galerie/clanky/TKOLYMP-nabor-FB-post-1200x630.png" />
      </div>

      <div className="col-feature my-16 grid lg:grid-cols-2 gap-4">
        <Map>
          {({ TileLayer, Marker, MapContainer, Popup }) => (
            <MapContainer className="min-h-48 min-w-24" center={{ lat:49.57963, lng:17.2495939 }} zoom={12} scrollWheelZoom={false}>
              <TileLayer url="https://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png" />
              <Marker position={{ lat: 49.57963, lng: 17.2495939 }}>
                <Popup>ZŠ Holečkova</Popup>
              </Marker>
            </MapContainer>
          )}
        </Map>

        <div className="prose prose-accent py-8">
          <h3>Taneční centrum při FZŠ Holečkova</h3>
          <p>
            Holečkova 10, 779 00, Olomouc<br/>
            (vchod brankou u zastávy Povel - škola)
          </p>
          <a href="https://www.zsholeckova.cz/" target="_blank">https://www.zsholeckova.cz/</a>
          <a href= "https://goo.gl/maps/swv3trZB2uvjcQfR6" target="_blank">Otevřít mapu</a>
        </div>
      </div>

      <div className="col-feature my-8">
        <img src="https://tkolymp.cz/galerie/clanky/prijdtancit2.jpg" />
      </div>

      <CallToAction url="/prijd-tancit" />
    </div>
  );
}
