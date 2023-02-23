import * as React from 'react';
import { CallToAction } from 'components/CallToAction';
import { Layout } from 'components/layout/Layout';
import { Heading } from 'components/Heading';
import { YoutubeEmbed } from 'components/YoutubeEmbed';

export default function ExtracurricularsPage() {
  return <>
    <Heading
      text="Přijď tančit!"
      image="https://tkolymp.cz/galerie/clanky/prijdtancit2.jpg"
      color={{ r: 216, g: 28, b: 58, a: 0.4 }}
    />

    <div className="mt-16 prose text-center">
      <h1>Přijďte si vyzkoušet, jak vypadá taneční trénink!</h1>
    </div>

    <div className="col-feature my-8 grid lg:grid-cols-2 gap-4">
      <div className="prose">
        <h2>Na co se můžete těšit?</h2>
        <ul>
          <li>Výukové lekce standardních a latinskoamerických tanců</li>
          <li>Můžete se přijít podívat na kteroukoli hodinu, stačí se přihlásit trenérovi před začátkem hodiny.</li>
          <li>První hodina <b>ZDARMA</b> po nezávazném přihlášení</li>
        </ul>
      </div>

      <div className="prose">
        <h2>Víte, že...?</h2>
        <ul>
          <li>Taneční sport je nově olympijským sportem</li>
          <li>Sportovcům přináší řadu možností a skvělých zážitků</li>
          <li>Většina účinkujících ve StarDance se věnovalo tanečnímu sportu</li>
          <li>Naučí děti základy time managentu - zorganizovat školu, trénink na ostatní aktivity</li>
          <li>Tanečníci se mohou stát členy národního reprezentačního týmu na reprezentovat tak ČR v zahraničí</li>
        </ul>
      </div>
    </div>

    <img src="https://tkolymp.cz/galerie/clanky/Týmové-foto-1.jpg" />


    <div className="col-feature my-8 grid lg:grid-cols-2 gap-4">
      <YoutubeEmbed title="" thumbnail="https://i3.ytimg.com/vi/MSahpLevS2g/maxresdefault.jpg">
        <iframe frameBorder="0" allowFullScreen allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture" width="100%" height="100%" src="https://www.youtube.com/embed/MSahpLevS2g?autoplay=1&amp;mute=0&amp;controls=1&amp;origin=https%3A%2F%2Ftkolymp.cz&amp;playsinline=1&amp;showinfo=0&amp;rel=0&amp;iv_load_policy=3&amp;modestbranding=1&amp;enablejsapi=1&amp;widgetid=23"></iframe>
      </YoutubeEmbed>

      <div className="prose">
        <h3>Znáte StarDance? A připadá vám někdy neuvěřitelné naučit se těžké kroky do velkého množství tanců?</h3>
        <h3>My to Vaše děti umíme naučit! Umíme z nich vychovat vrcholové sportovce!</h3>
      </div>
    </div>

    <CallToAction />
  </>;
};

ExtracurricularsPage.getLayout = (page: React.ReactElement) => <Layout showTopMenu>{page}</Layout>;
