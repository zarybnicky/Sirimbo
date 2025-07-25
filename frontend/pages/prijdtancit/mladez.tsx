import Map from '@/ui/map';
import { ProspectForm } from '@/ui/forms/ProspectForm';
import { Layout } from '@/components/layout/Layout';
import { NextSeo } from 'next-seo';
import * as React from 'react';
import LiteYouTubeEmbed from 'react-lite-youtube-embed';

export default function RecruitmentYouthPage() {
  const scrollToForm = (e: React.MouseEvent<HTMLAnchorElement>) => {
    e.preventDefault();
    document.querySelector(e.currentTarget.getAttribute('href') || '#form')?.scrollIntoView({
      behavior: 'smooth'
    });
  };
  return (
    <Layout showTopMenu>
      <NextSeo title="Přijď tančit!" />
      <div className="col-feature my-8 text-center">
        <img alt="" src="https://files.rozpisovnik.cz/file/rozpisovnik/tkolymp/1696525490988-TKOLYMP-nabor-FB-uvod-820x462.png" />
      </div>

      <div className="col-feature my-8 grid lg:grid-cols-2 gap-4">
        <div className="prose prose-accent">
          <h2>Na co se můžete těšit?</h2>
          <ul>
            <li>Výukové lekce standardních a latinskoamerických tanců</li>
            <li>Můžete se přijít podívat na kteroukoli hodinu, stačí vyplnit nezávazný přihlašovací formulář níže</li>
            <li><b>První hodina ZDARMA po nezávazném přihlášení</b></li>
            <li>Cena 2600 Kč za pololetí</li>
          </ul>
        </div>

        <div className="prose prose-accent">
          <h2>Kdy můžete přijít?</h2>
          <ul>
            <li><b>Pondělí 19:00-19:45</b></li>
            <li><b>Úterý 18:15-19:00</b></li>
          </ul>

          <h2>Kde nás najdete?</h2>
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
            <li>Tanec vytváří skvělý kolektiv a motivační prostředí</li>
            <li>Sportovcům přináší řadu možností a skvělých zážitků</li>
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
            <h2>Proč začít po tanečních?</h2>
          </div>
          <LiteYouTubeEmbed id="iKX-a2I5RIQ" adNetwork={true} params="modestbranding=1" poster="hqdefault" title="YouTube Embed" />
        </div>
      </div>

      <div className="col-feature my-16 grid lg:grid-cols-2 gap-4">
        <div className="prose prose-accent">
          <h2>Jak vypadá taneční trénink?</h2>
          <ul>
            <li>Hodina trvá 45 minut, přijďte 10-15 minut předem, k dispozici je šatna pro převléknutí</li>
            <li>Trenéři: Pavel Grepl a Hana Anna Šišková</li>
            <li>Náplň: technika standardních a latinskoamerických tanců</li>
            <li>Oblečení: kalhoty/sukně a tričko, pohodlné oblečení</li>
          </ul>
        </div>

        <div className="prose prose-accent">
          <h2>Co vás čeká...?</h2>
          <ul>
            <li>Choreografie a technika šesti tanců (Waltz, Valčík, Quickstep, Samba, Chacha, Jive)</li>
            <li>Rozvíjení tělesné zdatnosti a správného držení těla</li>
            <li>Rozvíjení správných pohybových návyků</li>
            <li>Trenér národního reprezentačního týmu (každý týden)</li>
            <li>Pravidelná jednodenní víkendová soustředění</li>
          </ul>
        </div>
      </div>

      <div className="text-4xl text-accent-0 text-center font-bold rounded-2xl p-4 shadow-md bg-accent-9 hover:bg-accent-10">
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
          <h2>Příběh tanečníka</h2>
          <p>Pavel začal tancovat v roce 2012 po absolvování základních tanečních kurzů.</p>
          <p>Po 4 letech se protančil do mezinárodní taneční třídy a mohl tak startovat na mistrovství České republiky.</p>
          <p>V následujících letech se díky spojení tance se studiem na Fakultě tělesné kultury stal respektovaným trenérem a podílí se na fungování národního reprezentačního týmu.</p>
          <p>Díky tomu, že se Pavel nebál a přišel po tanečních k nám, tak jeho život nabral nový směr a díky tanci si našel skvělou práci a splnil si dětský sen být trenérem.</p>
        </div>

        <div>
          <LiteYouTubeEmbed id="p9DJqmH-gTE" adNetwork={true} params="modestbranding=1" poster="hqdefault" title="YouTube Embed" />
        </div>
      </div>

      <div className="my-8 prose prose-accent text-center">
        <h2>Nečekejte na další pobídku, chyťte se příležitosti!</h2>
      </div>

      <div className="col-feature my-8 grid lg:grid-cols-2 items-end gap-4">
        <div className="aspect-w-16 aspect-h-9">
          <img className="object-cover" alt="" src="https://tkolymp.cz/galerie/clanky/Druzstva2019STTgroupmakrlik.jpg" />
        </div>

        <div>
          <div className="prose prose-accent mb-4">
            <h2>Z prodloužené k medailím</h2>
          </div>
          <LiteYouTubeEmbed id="w9NaFDOyJdk" adNetwork={true} params="modestbranding=1" poster="hqdefault" title="YouTube Embed" />
        </div>
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
          <a href="https://www.zsholeckova.cz/" target="_blank" rel="noreferrer">https://www.zsholeckova.cz/</a>
          <a href="https://goo.gl/maps/swv3trZB2uvjcQfR6" target="_blank" rel="noreferrer">Otevřít mapu</a>
        </div>
      </div>
    </Layout>
  );
};
