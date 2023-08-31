import { ExhibitionRequestForm } from '@app/ui/ExhibitionRequestForm';
import { TitleBar } from '@app/ui/TitleBar';
import { YoutubeEmbed } from '@app/ui/YoutubeEmbed';
import { Layout } from '@/components/layout/Layout';

const Page = () => {
  return (
    <Layout showTopMenu>
      <TitleBar title="Taneční vystoupení" />

      <YoutubeEmbed
        title=""
        thumbnail="https://i3.ytimg.com/vi/VsAgAfc9ZM4/maxresdefault.jpg"
      >
        <iframe
          allowFullScreen
          allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture"
          width="100%"
          height="100%"
          src="https://www.youtube.com/embed/VsAgAfc9ZM4?autoplay=1&amp;mute=0&amp;controls=1&amp;origin=https%3A%2F%2Ftkolymp.cz&amp;playsinline=1&amp;showinfo=0&amp;rel=0&amp;iv_load_policy=3&amp;modestbranding=1&amp;enablejsapi=1&amp;widgetid=23"
        ></iframe>
      </YoutubeEmbed>

      <div className="prose prose-accent mt-8">
        <p>
          Hledáte taneční vystoupení na svůj ples, firemní večírek nebo jinou společenskou
          akci? Máme pro Vás řešení!
        </p>

        <ul>
          <li>Standardní a latinskoamerické tance</li>
          <li>Mistři a vicemistři ČR a finalisty mezinárodních soutěží</li>
          <li>Všechny výkonnostní úrovně</li>
          <li>Věkové kategorie 12-20 let</li>
          <li>Délka vystoupení 6-10 minut za jeden vstup</li>
          <li>Cena je již od 2000 Kč</li>
        </ul>

        <p>
          Můžete nás kontaktovat na 737 545 525 nebo nezávazně vyplnit formulář a my
          se Vám ozveme.
        </p>
      </div>

      <div className="mt-8 mb-16">
        <ExhibitionRequestForm />
      </div>
    </Layout>
  );
}

export default Page;
