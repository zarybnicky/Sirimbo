import { ExhibitionRequestForm } from 'components/ExhibitionRequestForm';
import { Heading } from 'components/Heading';
import { NextSeo } from 'next-seo';
import type { NextPageWithLayout } from 'pages/_app';

const Page: NextPageWithLayout = () => {
  return (
    <>
      <Heading>Taneční vystoupení</Heading>
      <NextSeo title="Taneční vystoupení" />

      <div className="mb-8 prose prose-accent">
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

      <ExhibitionRequestForm />
    </>
  );
}

Page.showTopMenu = true;

export default Page;
