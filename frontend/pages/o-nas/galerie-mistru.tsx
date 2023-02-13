import * as React from 'react';
import { CallToAction } from 'components/CallToAction';
import { SlateEditor } from 'components/Slate';
import { Layout } from 'components/layout/Layout';
import { Heading } from 'components/Heading';
import { ServiceCard } from 'components/cards/ServiceCard';

export default function ChampionsPage() {
  return <>
    <Heading
      text="Galerie mistrů"
      image="https://tkolymp.cz/galerie/clanky/Sn%C3%ADmek-obrazovky-2022-08-29-v%C2%A011.26.17.png"
      color={{ r: 216, g: 28, b: 58, a: 0.4 }}
    />

    <div className="col-popout">
      {couples.map(couple => (
        <ServiceCard key={couple.header} header={couple.header} image={couple.image}>
          <SlateEditor value={couple.content} />
        </ServiceCard>
      ))}
    </div>

    <div className="my-16 prose text-center">
      <h1>Staň se dalším Mistrem ty!</h1>
    </div>

    <CallToAction />
  </>;
};

ChampionsPage.getLayout = (page: React.ReactElement) => <Layout showTopMenu>{page}</Layout>;

const mirek = {
  "header": "Miroslav Hýža - Denisa Walterová",
  "image": "https://tkolymp.cz/galerie/clanky/Týmové-foto-1.jpg",
  content: [
    {
      "type": "HEADINGS/HEADING-THREE",
      "children": [
        {
          "text": "Mistři ČR Junioři I 2000 – Standardní tance (Pardubice)",
          "EMPHASIZE/STRONG": true
        }
      ]
    },
    {
      "type": "HEADINGS/HEADING-THREE",
      "children": [
        {
          "text": "Mistři ČR Junioři I 2000 – Latinskoamerické tance (Ostrava)",
          "EMPHASIZE/STRONG": true
        }
      ]
    },
    {
      "type": "HEADINGS/HEADING-THREE",
      "children": [
        {
          "text": "Mistři ČR Junioři I 2000 – 10 tanců (Brno)",
          "EMPHASIZE/STRONG": true
        }
      ]
    }
  ]
};

const boruvci = {
  "header": "Daniel Borůvka - Barbora Borůvková",
  "image": "https://tkolymp.cz/galerie/clanky/286188899543293480751554738291220210599381o.jpg",
  content: [
    {
      "type": "HEADINGS/HEADING-THREE",
      "children": [
        {
          "text": "Mistři ČR Junioři I 2015 – Standardní tance (Otrokovice)"
        }
      ]
    },
    {
      "type": "HEADINGS/HEADING-THREE",
      "children": [
        {
          "text": "Mistři ČR Junioři I 2016 – Standardní tance (Praha)"
        }
      ]
    },
    {
      "type": "HEADINGS/HEADING-THREE",
      "children": [
        {
          "text": "Mistři ČR Junioři I 2016 – Latinskoamerické tance (Olomouc)"
        }
      ]
    },
    {
      "type": "HEADINGS/HEADING-THREE",
      "children": [
        {
          "text": "Mistři ČR Junioři I 2016 – 10 tanců (Zlín)"
        }
      ]
    },
    {
      "type": "HEADINGS/HEADING-THREE",
      "children": [
        {
          "text": "Mistři ČR Junioři II 2016 – 10 tanců (Zlín)"
        }
      ]
    }
  ]
};

const roman = {
  "header": "Roman Pecha - Eliška Koldová",
  "image": "https://tkolymp.cz/galerie/clanky/297501089656914702722766426355566753321169o.jpg",
  content: [
    {
      "type": "HEADINGS/HEADING-THREE",
      "children": [
        {
          "text": "Mistři ČR U21 2018 – Standardní tance (Třinec)"
        }
      ]
    },
    {
      "type": "HEADINGS/HEADING-THREE",
      "children": [
        {
          "text": "Mistři ČR U21 2018 – 10 tanců (Kojetín)"
        }
      ]
    }
  ]
};

const veverka = {
  "header": "Dominik Veverka - Aneta Zymová",
  "image": "https://tkolymp.cz/galerie/clanky/1721220812868224647286412094096557593334562o.jpg",
  content: [
    {
      "type": "HEADINGS/HEADING-THREE",
      "children": [
        {
          "text": "Mistři ČR Junioři I 2018 – Standardní tance (Třinec)"
        }
      ]
    }
  ]
};

const sirovi = {
  "header": "Vilém Šír - Anna Slouková",
  "image": "https://tkolymp.cz/galerie/clanky/1709720611681431199719363523339610656928229o.jpg",
  content: [
    {
      "type": "HEADINGS/HEADING-THREE",
      "children": [
        {
          "text": "Mistři ČR U21 2017 – Latinskoamerické tance (Praha)"
        }
      ]
    }
  ]
};

const selsdon = {
  "header": "Kaiem Selsdon - Viktorie Vágnerová",
  "image": "https://tkolymp.cz/galerie/clanky/27773020721464630255284428646841465494972563n.jpg",
  content: [
    {
      "type": "HEADINGS/HEADING-THREE",
      "children": [
        {
          "text": "Mistři ČR Junioři I 2022 – Latinskoamerické tance (Praha)"
        }
      ]
    }
  ]
};

const couples = [mirek, boruvci, roman, veverka, sirovi, selsdon];
