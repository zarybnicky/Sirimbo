import { TitleBar } from '@/ui/TitleBar';
import { ServiceCard } from '@/ui/cards/ServiceCard';
import * as React from 'react';
import { Layout } from '@/components/layout/Layout';

const Page = () => {
  return (
    <Layout showTopMenu>
      <TitleBar title="Galerie mistrů" />

      <div className="col-popout">
        {couples.map((couple) => (
          <ServiceCard key={couple.header} header={couple.header} image={couple.image}>
            {couple.content}
          </ServiceCard>
        ))}
      </div>

      <div className="my-16 prose prose-accent text-center">
        <h1>Staň se dalším Mistrem ty!</h1>
      </div>
    </Layout>
  );
}

export default Page;

const mirek = {
  header: 'Miroslav Hýža - Denisa Walterová',
  image: 'https://tkolymp.cz/galerie/clanky/Týmové-foto-1.jpg',
  content: (
    <div className="prose prose-accent">
      <p>Mistři ČR Junioři I 2000 – Standardní tance (Pardubice)</p>
      <p>Mistři ČR Junioři I 2000 – Latinskoamerické tance (Ostrava)</p>
      <p>Mistři ČR Junioři I 2000 – 10 tanců (Brno)</p>
    </div>
  ),
};

const boruvci = {
  header: 'Daniel Borůvka - Barbora Borůvková',
  image:
    'https://tkolymp.cz/galerie/clanky/286188899543293480751554738291220210599381o.jpg',
  content: (
    <div className="prose prose-accent">
      <p>Mistři ČR Junioři I 2015 – Standardní tance (Otrokovice)</p>
      <p>Mistři ČR Junioři I 2016 – Standardní tance (Praha)</p>
      <p>Mistři ČR Junioři I 2016 – Latinskoamerické tance (Olomouc)</p>
      <p>Mistři ČR Junioři I 2016 – 10 tanců (Zlín)</p>
      <p>Mistři ČR Junioři II 2017 – 10 tanců (Zlín)</p>
    </div>
  ),
};

const roman = {
  header: 'Roman Pecha - Eliška Koldová',
  image:
    'https://tkolymp.cz/galerie/clanky/297501089656914702722766426355566753321169o.jpg',
  content: (
    <div className="prose prose-accent">
      <p>Mistři ČR U21 2018 – Standardní tance (Třinec)</p>
      <p>Mistři ČR U21 2018 – 10 tanců (Kojetín)</p>
    </div>
  ),
};

const veverka = {
  header: 'Dominik Veverka - Aneta Zymová',
  image:
    'https://tkolymp.cz/galerie/clanky/1721220812868224647286412094096557593334562o.jpg',
  content: (
    <div className="prose prose-accent">
      <p>Mistři ČR Junioři I 2018 – Standardní tance (Třinec)</p>
    </div>
  ),
};

const sirovi = {
  header: 'Vilém Šír - Anna Slouková',
  image:
    'https://tkolymp.cz/galerie/clanky/1709720611681431199719363523339610656928229o.jpg',
  content: (
    <div className="prose prose-accent">
      <p>Mistři ČR U21 2017 – Latinskoamerické tance (Praha)</p>
    </div>
  ),
};

const selsdon = {
  header: 'Kaiem Selsdon - Viktorie Vágnerová',
  image: 'https://files.rozpisovnik.cz/file/rozpisovnik/tkolymp/1687512915639-Mistri-Selsdon.jpg',
  content: (
    <div className="prose prose-accent">
      <p>Mistři ČR Junioři I 2022 – Latinskoamerické tance (Praha)</p>
      <p>Mistři ČR Junioři I 2023 – Standardní tance</p>
      <p>Mistři ČR Junioři I 2023 – Latinskoamerické tance</p>
      <p>Mistři ČR Junioři I 2023 – 10 tanců</p>
    </div>
  ),
};

const couples = [mirek, boruvci, roman, veverka, sirovi, selsdon];
