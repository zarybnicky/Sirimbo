import { ExhibitionRequestForm } from '@/ui/ExhibitionRequestForm';
import { TitleBar } from '@/ui/TitleBar';
import { Layout } from '@/components/layout/Layout';
import LiteYouTubeEmbed from 'react-lite-youtube-embed';

const Page = () => {
  return (
    <Layout showTopMenu>
      <TitleBar title="Taneční vystoupení" />

      <div>
        <LiteYouTubeEmbed id="VsAgAfc9ZM4" adNetwork={true} params="modestbranding=1" poster="hqdefault" title="YouTube Embed" />
      </div>

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
          <li>Cena je již od 3000 Kč</li>
        </ul>

        <p>
          Můžete nás kontaktovat na 737 644 899 nebo nezávazně vyplnit formulář a my se Vám ozveme.
        </p>
      </div>

      <div className="mt-8 mb-16">
        <ExhibitionRequestForm />
      </div>
    </Layout>
  );
}

export default Page;
