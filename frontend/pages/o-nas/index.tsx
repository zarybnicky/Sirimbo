import * as React from 'react';
import { CallToAction } from 'components/CallToAction';
import { SlateEditor } from 'components/Slate';
import { Layout } from 'components/layout/Layout';
import { Heading } from 'components/Heading';

export default function AboutPage() {
  return <>
    <Heading
      text="O nás"
      image="https://tkolymp.cz/galerie/clanky/289541739553776446369923521898551803548994o.jpg"
      color={{ r: 111, g: 61, b: 1, a: 0.7 }}
    />

    <div className="col-feature my-16 grid gap-16 px-4 md:grid-cols-3">
      <div className="flex items-center justify-center">
        <img src="https://tkolymp.cz/galerie/clanky/293547069655557869525116228763683690860426o.jpg" />
      </div>
      <div className="col-span-2 -order-1 md:order-1">
        <SlateEditor readOnly value={text} />
      </div>
    </div>

    <CallToAction />
  </>;
};

AboutPage.getLayout = (page: React.ReactElement) => <Layout showTopMenu>{page}</Layout>;

const text = [
  {
    type: 'heading-one',
    children: [
      { text: 'OLYMP', primary: true, bold: true },
      { text: ' V TANEČNÍM SVĚTĚ', bold: true },
    ]
  },
  {
    "type": "PARAGRAPH/PARAGRAPH",
    "children": [
      {
        "text": "Taneční klub byl jedním z prvních klubů v České republice, který v počátku devadesátých let 20. století začal systematicky zaměřovat na práci s dětmi a mládeží. Jsme klubem s více než třicetiletou tradicí. Zabýváme se výchovou tanečních sportovců od dětí až po dospělé. Vytvořili jsme provázaný systém téninkových programů pro začínající, výkonnostní i vrcholoví sportovce. Využíváme moderní tréninkové metody a pravidelně je zdokonalujeme ve spolupráci s odborníky z Fakulty tělesné kultury Univerzity Palackého v Olomouci a mezinárodními trenéry World Dance Sport Organisation"
      }
    ]
  },
  {
    "type": "PARAGRAPH/PARAGRAPH",
    "children": [
      {
        "text": "V rámci Českého svazu tanečního sportu jsme jediným klubem z Olomouckého kraje se statutem Sportovního centra mládeže. Pravidelně dodáváme členy národnímu reprezentačnímu týmu a dlouhodobě patříme mezi nejlepší české kluby v práci s dětmi a mládeží. "
      }
    ]
  },
  {
    "type": "PARAGRAPH/PARAGRAPH",
    "children": [
      {
        "text": "Působíme v Olomouci a Prostějově. "
      }
    ]
  }
]
