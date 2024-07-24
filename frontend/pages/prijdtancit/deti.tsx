import Map from '@/map';
import { ProspectForm } from '@/ui/forms/ProspectForm';
import { Layout } from '@/components/layout/Layout';
import { NextSeo } from 'next-seo';
import * as React from 'react';
import LiteYouTubeEmbed from 'react-lite-youtube-embed';
import { ArrowRight } from 'lucide-react';

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
      <div className="col-feature mt-8 text-center">
        <img className="inline max-h-[400px]" alt="" src="https://files.rozpisovnik.cz/file/rozpisovnik/tkolymp/1696525490988-TKOLYMP-nabor-FB-uvod-820x462.png" />
      </div>

      <div className="col-feature my-8 grid lg:grid-cols-2 gap-4">
        <div className="prose prose-accent">
          <h2>Na co se můžete těšit?</h2>
          <ul>
            <li>Výukové lekce standardních a latinskoamerických tanců</li>
            <li>Můžete se přijít podívat na kteroukoli hodinu, stačí vyplnit nezávazný přihlašovací formulář níže</li>
            <li>Cena <b>2900{'\u00A0'}Kč</b> za pololetí</li>
          </ul>
        </div>

        <div className="prose prose-accent">
          <h2>Kdy můžete přijít?</h2>
          <p>Tréninky probíhají každý týden:</p>
          <ul>
            <li>v pondělí a ve středu v 17:30 hodin</li>
          </ul>
          <p><b>Zahájení tréninků - pondělí 9. září</b></p>
        </div>
      </div>

      <div className="col-feature my-4 grid lg:grid-cols-2 gap-4">
        <div>
          <div className="bg-accent-11 rounded-2xl p-4 mr-2">
            <div className="prose prose-accent text-white">
              <h3 className="text-white mt-0">Akce</h3>
              <ul>
                <li>Při zápisu a zaslání platby <b>do 31. 8. 2024</b> je cena za pololetí <b>2600{'\u00A0'}Kč</b>.</li>
                <li>Platební informace: č.ú.: 1806875329/0800, VS: rodné číslo, SS: 20240831, poznámka: Jméno a příjmení člena</li>
              </ul>
            </div>

            <div className="mt-3 mr-2 text-2xl text-accent-0 text-center rounded-2xl p-4 shadow-md bg-accent-9 hover:bg-accent-10">
              <a href="#form" onClick={scrollToForm}>
                <h1>CHCI SLEVU <ArrowRight className="inline-block size-6 -mt-1" /></h1>
              </a>
            </div>
          </div>
        </div>

        <div className="prose prose-accent">
          <h2>Kde nás najdete?</h2>
          <p>
            <b>Taneční centrum při FZŠ Holečkova</b><br />
            Holečkova 10, 779 00, Olomouc<br />
            (vchod brankou u zastávky Povel - škola)
          </p>

          <a href="https://www.zsholeckova.cz/" target="_blank">https://www.zsholeckova.cz/</a><br/>
          <a href="https://goo.gl/maps/swv3trZB2uvjcQfR6" target="_blank">Otevřít mapu</a>
        </div>
      </div>

      <div className="col-feature my-4 grid lg:grid-cols-2 gap-4">
        <div className="prose prose-accent">
          <h2>Víte, že...?</h2>
          <ul>
            <li>Taneční sport je nově olympijským sportem</li>
            <li>Sportovcům přináší řadu možností a skvělých zážitků</li>
            <li>Naučí děti základy time managementu - zorganizovat školu, trénink a ostatní aktivity</li>
            <li>Tanečnímu sportu se věnovala většina účinkujících ve StarDance</li>
            <li>Tanečníci se mohou stát členy národního reprezentačního týmu a reprezentovat tak ČR v zahraničí</li>
          </ul>
        </div>

        <img alt="" src="https://tkolymp.cz/galerie/clanky/Týmové-foto-1.jpg" />
      </div>

      <div className="col-feature my-16 grid lg:grid-cols-2 gap-4">
        <div>
          <div className="prose prose-accent mb-4">
            <h2>Proč začít tancovat?</h2>
          </div>
          <LiteYouTubeEmbed id="WR9ZVW-tezc" adNetwork={true} params="start=21&modestbranding=1" poster="hqdefault" title="YouTube Embed" />
        </div>
        <div>
          <div className="prose prose-accent mb-4">
            <h2>Poznej taneční sport</h2>
          </div>
          <LiteYouTubeEmbed id="q83_AeIm8Mw" adNetwork={true} params="modestbranding=1" poster="hqdefault" title="YouTube Embed" />
        </div>
      </div>

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
            <li>Oblečení: volné sportovní, sálová obuv (sálové tenisky do tělocviku)</li>
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
            <li>Vánoční besídka s odměnou (prosinec), vystoupení na Akademii Olymp Dance (leden), rodinné vstupné na taneční soutěž pro celou rodinu zdarma (květen)</li>
          </ul>
        </div>
      </div>

      <div className="text-3xl hover:underline text-accent-0 text-center rounded-2xl p-4 shadow-md bg-accent-9 hover:bg-accent-10">
        <a href="#form" onClick={scrollToForm}>
          <h1>ZAPIŠ SE NA PRVNÍ HODINU ZDARMA!</h1>
        </a>
      </div>

      <div className="col-feature my-16 grid lg:grid-cols-2 gap-4">
        <div>
          <LiteYouTubeEmbed id="flkU9ZeM7_8" adNetwork={true} params="modestbranding=1" poster="hqdefault" title="YouTube Embed" />
        </div>

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

        <div>
          <LiteYouTubeEmbed id="lURCOEiVbGc" adNetwork={true} params="modestbranding=1" poster="hqdefault" title="YouTube Embed" />
        </div>
      </div>

      <div className="my-8 prose prose-accent text-center">
        <h2>
          Nečekejte, až vaše děti vyrostou, vrcholoví sportovci začínají již v dětském
          věku.
        </h2>
      </div>

      <div className="col-feature my-8 grid lg:grid-cols-2 items-center gap-4">
        <div className="aspect-w-16 aspect-h-9">
          <img className="object-cover" alt="" src="https://tkolymp.cz/galerie/clanky/Druzstva2019STTgroupmakrlik.jpg" />
        </div>

        <div>
          <LiteYouTubeEmbed id="TOrHc7JYUac" adNetwork={true} params="modestbranding=1" poster="hqdefault" title="YouTube Embed" />
        </div>
      </div>

      <div id="form" className="my-8">
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
          <a href="https://www.zsholeckova.cz/" target="_blank">https://www.zsholeckova.cz/</a><br/>
          <a href="https://goo.gl/maps/swv3trZB2uvjcQfR6" target="_blank">Otevřít mapu</a>
        </div>
      </div>
    </Layout>
  );
};

export default Page;
