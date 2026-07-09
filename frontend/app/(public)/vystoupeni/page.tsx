/* eslint-disable import-x/no-unused-modules */
import { createPublicPageMetadata } from '@/lib/seo';
import LiteYouTubeEmbed from '@/ui/LiteYouTubeEmbed';
import { ExhibitionRequestForm } from '@/ui/forms/ExhibitionRequestForm';
import { PageHeader } from '@/ui/TitleBar';

export const metadata = createPublicPageMetadata({
  title: 'Taneční vystoupení',
  description:
    'Objednejte taneční vystoupení TK Olymp na ples, firemní večírek nebo společenskou akci. Standardní i latinskoamerické tance, soutěžní páry a jednoduchá poptávka.',
  path: '/vystoupeni',
});

export default function Vystoupeni() {
  return (
    <>
      <PageHeader title="Taneční vystoupení" />

      <div>
        <LiteYouTubeEmbed
          id="VsAgAfc9ZM4"
          adNetwork={true}
          params="modestbranding=1"
          poster="hqdefault"
          title="YouTube Embed"
        />
      </div>

      <div className="prose prose-accent mt-8">
        <p>
          Hledáte taneční vystoupení na svůj ples, firemní večírek nebo jinou společenskou
          akci? Máme pro Vás řešení!
        </p>

        <ul>
          <li>Standardní a latinskoamerické tance</li>
          <li>Mistři a vicemistři ČR a finalisté mezinárodních soutěží</li>
          <li>Všechny výkonnostní úrovně</li>
          <li>Věkové kategorie 12-20 let</li>
          <li>Délka vystoupení 6-10 minut za jeden vstup</li>
          <li>Cena je již od 3000 Kč</li>
        </ul>

        <p>
          Můžete nás kontaktovat na 737 644 899 nebo nezávazně vyplnit formulář a my se
          Vám ozveme.
        </p>
      </div>

      <div className="mt-8 mb-16">
        <ExhibitionRequestForm />
      </div>
    </>
  );
}
