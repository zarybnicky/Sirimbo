import * as React from 'react';
import { CallToAction } from 'components/CallToAction';
import { SlateReadonly } from 'components/SlateReadonly';

import MirekDomca from 'public/images/mirek-domca.png';

export default function AboutPage() {
  return <>
    <div className="container mx-auto max-w-3xl mt-8 mb-8">
      <div className="grid gap-4 px-4 md:grid-cols-3">
        <div className="flex items-center justify-center">
          <img src={MirekDomca.src} alt="Miroslav Hýža a Dominika Feglová" />
        </div>
        <div className="col-span-2 -order-1 md:order-1">
          <SlateReadonly value={text} />
        </div>
      </div>
    </div>
    <CallToAction />
  </>;
};

const text = [
  {
    type: 'heading-two',
    children: [
      { text: 'OLYMP', primary: true, bold: true },
      { text: ' V TANEČNÍM SVĚTĚ', bold: true },
    ]
  },
  {
    type: 'paragraph',
    children: [
      { text: 'Jsme tradičním klubem, který se věnuje tanečnímu sportu ', bold: true },
      { text: '- standardním a latinskoamerickým tancům. Více než třicet let se zabýváme se výchovou tanečních sportovců od dětí až po dospělé.' },
      { text: 'Vytvořili jsme provázaný systém tréninkových programů pro začínající, výkonnostní i vrcholové sportovce.' },
      { text: 'Využíváme nejmodernějších tréninkových metod a pravidelně je zdokonalujeme ve spolupráci s odborníky z Fakulty tělesné kultury Univerzity Palackého v Olomouci a mezinárodními trenéry' },
      { text: ' World Dance Sport Organisation.', bold: true },
    ],
  },
  {
    type: 'paragraph',
    children: [
      { text: 'V rámci Českého svazu tanečního sportu jsme jediným klubem z Olomouckého kraje se statutem Sportovní centrum mládeže MŠMT. Pravidelně dodáváme členy národnímu reprezentačnímu týmu a dlouhodobě patříme mezi nejlepší české kluby v práci s mládeží.' },
    ],
  },
  {
    type: 'paragraph',
    children: [
      { text: 'Působíme v Olomouci, Prostějově a Přerově.', bold: true },
    ],
  },
]
